(local wezterm _G.wezterm)
(local act wezterm.action)
(local mux wezterm.mux)
(local config (wezterm.config_builder))

(local project-dir (.. wezterm.home_dir :/Development))
(local project-dirname (string.gsub project-dir "(.*/)(.*)" "%2"))
(local project-dir-find-cmd [:fd
                             "'(\\.git|\\.jj|BUILD\\.bazel|workspace\\.yaml)$'"
                             project-dir
                             :-d
                             :8
                             :-H
                             :-X
                             :echo
                             "\"{//}\n\""])

(lambda table-concat [t1 t2]
  "Concatenate two tables"
  (local tc [])
  (each [_ n (ipairs t1)] (table.insert tc n))
  (each [_ n (ipairs t2)] (table.insert tc n))
  tc)

(lambda has-prefix [str prefix]
  "Check if the string starts with the given prefix"
  (= (str:sub 1 (length prefix)) prefix))

(lambda reverse-ipairs [t]
  "Create a reverse ipairs iterator for given table"
  (local tlen (length t))
  (values (fn [tbl idx]
            (let [val (. tbl (- tlen idx))]
              (when val
                (values (+ idx 1) val)))) t 0))

(lambda table-contains [tbl cmp]
  "If the given cmp function returns true for any item in the table, this function returns true"
  (var found false)
  (each [_ value (pairs tbl) &until found]
    (when (cmp value) (set found true)))
  found)

(lambda table-find [tbl cmp]
  "Return the first item for which the given cmp function returns true"
  (var item nil)
  (each [name value (pairs tbl) &until item]
    (when (cmp name value) (set item value)))
  item)

(lambda project-name [path]
  "Get the project/workspace name for the given path"
  (if (has-prefix path "/")
      (let [name (string.gsub path "(.*/)(.*)" "%2")
            dirname-path (string.gsub path "(.*)/(.*)" "%1")
            dirname (string.gsub dirname-path "(.*/)(.*)" "%2")]
        (if (not= dirname project-dirname) (.. dirname "/" name) name))
      path))

(lambda run-child-process [_window pane args]
  "Runs a child process in the given context (i.e ssh if an ssh domain)"
  (let [domain (pane:get_domain_name)
        args (accumulate [a args _ d (pairs config.ssh_domains)
                          &until (= :ssh (. a 1))]
               (if (= d.name domain)
                   [:ssh (.. d.username "@" d.remote_address) (table.unpack a)]
                   a))
        args (if (= (. args 1) :ssh) args
                 (icollect [_ a (ipairs args)] (. (wezterm.shell_split a) 1)))
        (status out err) (wezterm.run_child_process args)]
    (values status out err)))

(lambda spawn-project-window [_window pane]
  "Spawn a new window in the current project/workspace and domain"
  (let [domain (pane:get_domain_name)
        cwd (pane:get_current_working_dir)]
    (mux.spawn_window {:domain {:DomainName domain} :cwd cwd.file_path})))

(lambda spawn-project-tab [window pane]
  "Spawn a new tab in the current project/workspace and domain"
  (let [domain (pane:get_domain_name)
        cwd (pane:get_current_working_dir)]
    (-> (window:mux_window)
        (: :spawn_tab {:domain {:DomainName domain} :cwd cwd.file_path}))))

(lambda unique [list]
  (let [unique-items {}]
    (each [_ item (ipairs list)]
      (set (. unique-items item) true))
    (accumulate [list [] item _ (pairs unique-items)]
      (do
        (table.insert list item)
        list))))

(lambda project-directory-list [window pane]
  "Returns a list of paths to projects"
  (let [(_status out _err) (run-child-process window pane project-dir-find-cmd)
        listing []]
    (each [line (out:gmatch "[^\n]+")]
      (let [line (line:gsub "^%s*(.-)%s*$" "%1")]
        (table.insert listing line)))
    (unique listing)))

(local default-project-workspace-yaml-path
       (.. wezterm.config_dir :/workspace.yaml))

(lambda default-project-workspace []
  (let [(wsloaded err) (pcall #(with-open [wsyaml (io.open default-project-workspace-yaml-path
                                                           :rb)]
                                 (wezterm.serde.yaml_decode (wsyaml:read :*a))))]
    (if wsloaded err {:windows [{}]})))

(wezterm.log_info "default-project-workspace: " (default-project-workspace))

(local default-ssh-domains-yaml-path (.. wezterm.config_dir :/ssh_domains.yaml))

(lambda load-ssh-domains []
  (let [(sshloaded err) (pcall #(with-open [sshyaml (io.open default-ssh-domains-yaml-path
                                                             :rb)]
                                  (wezterm.serde.yaml_decode (sshyaml:read :*a))))]
    (if sshloaded err [])))

(lambda project-workspace-config [window pane dir]
  "Reads and returns the effective workspace config for a project"
  (wezterm.log_info "default-project-workspace " (default-project-workspace))
  (let [(status out _err) (run-child-process window pane
                                             [:cat (.. dir :/workspace.yaml)])]
    (if status (wezterm.serde.yaml_decode out) (default-project-workspace))))

(lambda project-jump-list [window pane]
  "Returns a jump list for use with the input selector"
  (let [workspaces (mux.get_workspace_names)
        wsmap (collect [_ w (ipairs workspaces)] w true)
        project-dirs (icollect [_ p (ipairs (project-directory-list window pane))]
                       (if (not (?. wsmap (project-name p))) p))
        workspaces-and-projects (icollect [_ p (ipairs (table-concat workspaces
                                                                     project-dirs))]
                                  {:id (project-name p) :label p})]
    workspaces-and-projects))

(lambda has-workspace [name]
  (table-contains (mux.get_workspace_names)
                  (fn [item]
                    (= name item))))

(lambda command-for [window-or-pane]
  (local pane-name (or window-or-pane.name :unknown))
  (local args [])
  (local pane-name-bash-cmd (.. "printf \"\\033];1337;SetUserVar=%s=%s\\007\" pane_name `echo -n "
                                pane-name
                                " | base64`; printf \"\\033]1;%s\\007\" "
                                pane-name "; "))
  (var cmd "")
  (if window-or-pane.command
      (do
        (table.insert args :bash)
        (table.insert args :-c)
        (set cmd (.. cmd pane-name-bash-cmd))
        (when (or (= window-or-pane.exit_to_shell nil)
                  (= window-or-pane.exit_to_shell true))
          (set cmd (.. cmd "trap \"exec $SHELL\" SIGINT; ")))
        (if window-or-pane.restart
            (set cmd (.. cmd "while true; do " window-or-pane.command
                         "; sleep 1; done"))
            (set cmd (.. cmd window-or-pane.command)))
        (table.insert args cmd))
      (do
        (table.insert args :bash)
        (table.insert args :-c)
        (table.insert args (.. pane-name-bash-cmd "exec $SHELL"))))
  (wezterm.log_info "command-for: " args)
  args)

(fn call-with-delay [items delay-seconds func]
  (when (and items (> (length items) 0))
    (fn process-item [index]
      (when (<= index (length items))
        ;; Call the function with the current item
        (func (. items index) index)
        ;; Schedule the next item
        (when (< index (length items))
          (wezterm.time.call_after delay-seconds #(process-item (+ index 1))))))

    ;; Start with delay before first item
    (wezterm.time.call_after delay-seconds #(process-item 1))))

(lambda build-workspace-layout [window pane domain directory panes-config]
  (wezterm.log_info "build-workspace-layout: " panes-config)
  (wezterm.log_info "window: " window)
  (wezterm.log_info "pane: " pane)
  (let [active_tab (window:active_tab)
        first_pane (window:active_pane)]
    (wezterm.log_info "set tab title: main")
    (run-child-process window pane [:wezterm
                                    :cli
                                    :set-tab-title
                                    :--tab-id
                                    (active_tab:tab_id)
                                    :main])
    (local panes [first_pane])
    (when panes-config
      (wezterm.log_info "panes: " panes-config)
      (call-with-delay panes-config 0.1
                       (fn [pane-conf _id]
                         (let [args (command-for pane-conf)
                               direction (or pane-conf.direction :Right)
                               size (or pane-conf.size 0.5)
                               from-pane (. panes
                                            (or pane-conf.split_from
                                                (length panes)))
                               new-pane (from-pane:split {:cwd directory
                                                          :domain {:DomainName domain}
                                                          : direction
                                                          : size
                                                          : args})]
                           (wezterm.log_info "pane-conf: " pane-conf)
                           (wezterm.log_info "args: " args " direction: "
                                             direction " size: " size)
                           (table.insert panes new-pane)))))))

(lambda setup-project-workspace [window pane name directory]
  (let [domain (pane:get_domain_name)]
    (window:perform_action (wezterm.action_callback (fn [window pane]
                                                      (if (has-workspace name)
                                                          (window:perform_action (act.SwitchToWorkspace {: name})
                                                                                 pane)
                                                          (let [workspace-config (project-workspace-config window
                                                                                                           pane
                                                                                                           directory)]
                                                            (each [_ window-conf (reverse-ipairs workspace-config.windows)]
                                                              (let [args (command-for window-conf)]
                                                                (window:perform_action (act.SwitchToWorkspace {: name
                                                                                                               :spawn {:domain {:DomainName domain}
                                                                                                                       :cwd directory
                                                                                                                       : args}})
                                                                                       pane)
                                                                (wezterm.time.call_after 0.5
                                                                                         (fn []
                                                                                           (build-workspace-layout window
                                                                                                                   pane
                                                                                                                   domain
                                                                                                                   directory
                                                                                                                   window-conf.panes)))))))))
                           pane)))

(lambda reload-workspace-action [window pane]
  (let [name (window:active_workspace)
        directory (pane:get_current_working_dir)]
    (each [_ _muxtab (ipairs (-> (window:mux_window) (: :tabs)))]
      (window:perform_action (act.CloseCurrentTab {:confirm false})
                             (window:active_pane)))
    (let [window (. (wezterm.gui.gui_windows) 1)
          pane (window:active_pane)]
      (setup-project-workspace window pane name directory.file_path))))

(lambda select-project-action-callback [window pane ?name ?directory]
  (if (not (and ?name ?directory))
      (wezterm.log_info "cancelled project selection")
      (do
        (wezterm.log_info "select project name: " ?name " dir: " ?directory)
        (setup-project-workspace window pane ?name ?directory))))

(lambda open-select-project-window [window pane]
  (let [jump-list (project-jump-list window pane)]
    (window:perform_action (act.InputSelector {:action (wezterm.action_callback select-project-action-callback)
                                               :title :Projects
                                               :choices jump-list
                                               :fuzzy true})
                           pane)))

(lambda open-named-tab-action [window _pane name]
  (each [_ muxtab (ipairs (-> (window:mux_window) (: :tabs)))]
    (when (= (muxtab:get_title) name)
      (muxtab:activate))))

(lambda open-context-tab-action [window pane name args]
  (wezterm.log_info "activate " name " tab action")
  (if (table-contains (-> (window:mux_window) (: :tabs))
                      (fn [tab] (print "tabname: " (tab:get_title))
                        (= (tab:get_title) name)))
      (if (= (-> (window:active_tab) (: :get_title)) name)
          (open-named-tab-action window pane :main)
          (open-named-tab-action window pane name)))
  (let [domain (pane:get_domain_name)
        cwd (pane:get_current_working_dir)]
    (pane window)
    (-> (window:mux_window)
        (: :spawn_tab {:cwd cwd.file_path :domain {:DomainName domain} : args}))))

(lambda pane-with-id [window pane-id]
  (var pane nil)
  (each [_ p (ipairs (or (-?> (window:active_tab) (: :panes)) []))]
    (when (= (p:pane_id) pane-id)
      (set pane p)))
  pane)

(lambda pane-with-name [window pane-name]
  (var pane nil)
  (each [_ p (ipairs (or (-?> (window:active_tab) (: :panes)) []))]
    (when (= (?. (p:get_user_vars) :pane_name) pane-name)
      (set pane p)))
  pane)

(lambda pane-name-for-id [window pane-id]
  (-?> (pane-with-id window pane-id) (: :get_user_vars) (. :pane_name)))

(lambda pane-id-for-name [window pane-name]
  (-?> (pane-with-name window pane-name) (: :pane_id)))

(lambda pane-information-for [pane]
  (var pane-info nil)
  (each [_ pinfo (ipairs (or (-?> (pane:tab) (: :panes_with_info)) []))]
    (when (= (pinfo.pane:pane_id) (pane:pane_id)) (set pane-info pinfo)))
  pane-info)

(lambda toggle-maximized-pane [pane-name]
  (lambda [window pane]
    (let [ws (window:active_workspace)
          tab (pane:tab)
          pane-id (pane-id-for-name window pane-name)
          pane-info (or (pane-information-for (pane-with-name window pane-name))
                        {})
          zoomed (or (?. pane-info :is_zoomed) false)]
      (wezterm.log_info "ws: " ws " tab: " tab " pane-id: " pane-id " zoomed: "
                        zoomed)
      (each [_ p (ipairs (tab:panes))]
        (when (= (p:pane_id) pane-id) (p:activate)
          (tab:set_zoomed (not zoomed)))))))

(wezterm.on :ReloadWorkspace reload-workspace-action)
(wezterm.on :ActivateGitui
            (lambda [window pane]
              (open-context-tab-action window pane :gitui [:gitui])))

(wezterm.on :ActivateLazyjj
            (lambda [window pane]
              (open-context-tab-action window pane :lazyjj [:lazyjj])))

(wezterm.on :ActivateSerpl
            (lambda [window pane]
              (open-context-tab-action window pane :serpl [:serpl])))

(wezterm.on :ToggleMaximizeTerminal (toggle-maximized-pane :term))
(wezterm.on :ToggleMaximizeEditor (toggle-maximized-pane :editor))
(wezterm.on :ToggleMaximizeAI (toggle-maximized-pane :aichat))
(wezterm.on :ActivateMainUI
            (lambda [window pane] (open-context-tab-action window pane :main)))

(wezterm.on :ReloadFixup
            (lambda [window pane]
              (run-child-process window pane [:pkill :-HUP :direnv])))

(wezterm.on :FindProject open-select-project-window)
(wezterm.on :NewProjectWindow spawn-project-window)
(wezterm.on :NewProjectTab spawn-project-tab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tab bar styling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The filled in variant of the < symbol
(local solid-left-arrow wezterm.nerdfonts.pl_right_hard_divider)

; The filled in variant of the > symbol
(local solid-right-arrow wezterm.nerdfonts.pl_left_hard_divider)

(local remote-dev-color "#bf616a")
(local local-dev-color "#5e81ac")
(local local-term-color "#4f8c82")

; (local text-fg "#c0c0c0")

(wezterm.on :format-tab-title
            (lambda [tab tabs _panes config _hover _max_width]
              (let [title (or tab.tab_title tab.active_pane.title)
                    first (= tab.tab_index 0)
                    last (= tab.tab_index (- (length tabs) 1))
                    tab-bg config.colors.tab_bar.background
                    active-bg-color "#51576d"
                    inactive-bg-color "#0b0022"
                    active-fg-color "#f9f6fc"
                    inactive-fg-color "#a6a4ac"]
                (if tab.is_active
                    (if first
                        [{:Background {:Color active-bg-color}}
                         {:Foreground {:Color active-bg-color}}
                         {:Attribute {:Intensity :Bold}}
                         {:Text " "}
                         {:Background {:Color active-bg-color}}
                         {:Foreground {:Color active-fg-color}}
                         {:Attribute {:Intensity :Bold}}
                         {:Text (.. (tostring (+ tab.tab_index 1)) ": " title
                                    " ")}
                         {:Background {:Color (if last tab-bg inactive-bg-color)}}
                         {:Foreground {:Color active-bg-color}}
                         {:Attribute {:Intensity :Bold}}
                         {:Text solid-right-arrow}]
                        [{:Background {:Color active-bg-color}}
                         {:Foreground {:Color inactive-bg-color}}
                         {:Attribute {:Intensity :Bold}}
                         {:Text solid-right-arrow}
                         {:Background {:Color active-bg-color}}
                         {:Foreground {:Color active-fg-color}}
                         {:Attribute {:Intensity :Bold}}
                         {:Text (.. " " (tostring (+ tab.tab_index 1)) ": "
                                    title " ")}
                         {:Background {:Color (if last tab-bg inactive-bg-color)}}
                         {:Foreground {:Color active-bg-color}}
                         {:Attribute {:Intensity :Bold}}
                         {:Text solid-right-arrow}])
                    (if first
                        [{:Background {:Color inactive-bg-color}}
                         {:Foreground {:Color inactive-bg-color}}
                         {:Attribute {:Intensity :Bold}}
                         {:Text " "}
                         {:Background {:Color inactive-bg-color}}
                         {:Foreground {:Color inactive-fg-color}}
                         {:Attribute {:Intensity :Bold}}
                         {:Text (.. (tostring (+ tab.tab_index 1)) ": " title
                                    " ")}]
                        [{:Background {:Color inactive-bg-color}}
                         {:Foreground {:Color inactive-bg-color}}
                         {:Attribute {:Intensity :Bold}}
                         {:Text " "}
                         {:Background {:Color inactive-bg-color}}
                         {:Foreground {:Color inactive-fg-color}}
                         {:Attribute {:Intensity :Bold}}
                         {:Text (.. (tostring (+ tab.tab_index 1)) ": " title
                                    " ")}
                         {:Background {:Color (if last tab-bg inactive-bg-color)}}
                         {:Foreground {:Color inactive-bg-color}}
                         {:Attribute {:Intensity :Bold}}
                         {:Text solid-right-arrow}])))))

(lambda insert-bar-item [items text fg-color bg-color ?last]
  (when (not ?last)
    (do
      (table.insert items {:Foreground {:Color bg-color}})
      (table.insert items {:Text solid-left-arrow})))
  (table.insert items {:Foreground {:Color fg-color}})
  (table.insert items {:Background {:Color bg-color}})
  (table.insert items {:Attribute {:Intensity :Bold}})
  (table.insert items {:Text (.. " " text " ")})
  items)

(wezterm.on :update-status
            (lambda [window pane]
              (var tab-bg remote-dev-color)
              (let [domain (pane:get_domain_name)
                    pane-info (pane-information-for pane)
                    zoomed (or (?. pane-info :is_zoomed) false)
                    overrides (or (window:get_config_overrides) {})
                    key-table (string.upper (string.gsub (or (window:active_key_table)
                                                             :normal)
                                                         :_mode$ ""))
                    (_ hostname _) (run-child-process window pane [:hostname])
                    ws (window:active_workspace)]
                (case domain
                  :local (set tab-bg local-term-color)
                  :local-dev (set tab-bg local-dev-color))
                (set overrides.colors {:tab_bar {:background tab-bg}})
                (window:set_config_overrides overrides)
                (let [elements []]
                  (do
                    (insert-bar-item elements "" "#000000" tab-bg)
                    (insert-bar-item elements (if zoomed " 👁 " "  ")
                                     "#FFFFFF" tab-bg true)
                    (insert-bar-item elements key-table "#FFFFFF" "#51576d")
                    (insert-bar-item elements ws "#000000" "#a6e3a1")
                    (insert-bar-item elements (.. domain "/" hostname)
                                     "#000000" "#f38ba8"))
                  (window:set_right_status (wezterm.format elements))))))

; (wezterm.on :gui-attached
;             (lambda [_domain]
;               (wezterm.log_info :gui-attached)
;               (let [ws (mux.get_active_workspace)]
;                 (wezterm.log_info "ws: " ws)
;                 (wezterm.log_info "windows: " (mux.all_windows))
;                 (wezterm.time.call_after 2
;                                          (fn []
;                                            (each [_ window (ipairs (mux.all_windows))]
;                                              (when (= (window:workspace) ws)
;                                                (let [w (window:gui_window)]
;                                                  (wezterm.emit :FindProject w
;                                                                (w:active_pane))))))))))

; wezterm.on('gui-attached', function(domain)
;   -- maximize all displayed windows on startup
;   local workspace = mux.get_active_workspace()
;   for _, window in ipairs(mux.all_windows()) do
;     if window:get_workspace() == workspace then
;       window:gui_window():maximize()
;     end
;   end
; end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|\
(set config.check_for_updates false)
(set config.mux_env_remove {})
(set config.adjust_window_size_when_changing_font_size false)
(set config.enable_wayland true)
(set config.enable_tab_bar true)
(set config.use_fancy_tab_bar false)
(set config.tab_max_width 64)
; no effect on wayland
(set config.max_fps 120)
(set config.show_tabs_in_tab_bar true)
(set config.show_new_tab_button_in_tab_bar false)
(set config.tab_bar_at_bottom true)
(set config.hide_tab_bar_if_only_one_tab false)
(set config.font (wezterm.font "JetBrainsMono Nerd Font"))
(set config.font_size 14)
(set config.color_scheme "Nord (Gogh)")
(set config.window_background_opacity 0.91)
; (set config.pane_focus_follows_mouse true)
(set config.switch_to_last_active_tab_when_closing_tab true)
(wezterm.add_to_config_reload_watch_list (.. project-dir
                                             :/world/users/profiles/wezterm/wezterm.fnl))

(wezterm.add_to_config_reload_watch_list (.. wezterm.home_dir
                                             :/.config/wezterm/wezterm.fnl.lua))

(set config.leader {:key :Space :mods :CTRL})
(set config.key_tables
     {:pane_mode [{:key :Escape :action act.PopKeyTable}
                  {:key :RightArrow
                   :action (act.SplitPane {:direction :Right
                                           :size {:Percent 50}})}
                  {:key :LeftArrow
                   :action (act.SplitPane {:direction :Left
                                           :size {:Percent 50}})}
                  {:key :DownArrow
                   :action (act.SplitPane {:direction :Down
                                           :size {:Percent 50}})}
                  {:key :UpArrow
                   :action (act.SplitPane {:direction :Up :size {:Percent 50}})}
                  {:key :p :action act.PaneSelect}
                  {:key :z :action act.TogglePaneZoomState}
                  {:key :q :action (act.CloseCurrentPane {:confirm false})}
                  {:key :Escape :action act.PopKeyTable}]
      :tab_mode [{:key :Escape :action act.PopKeyTable}
                 {:key :t :action (act.EmitEvent :NewProjectTab)}
                 {:key :q :action (act.CloseCurrentTab {:confirm false})}]
      :workspace_mode [{:key :Escape :action act.PopKeyTable}
                       {:key :r :action (act.EmitEvent :ReloadWorkspace)}
                       {:key :w :action (act.EmitEvent :FindProject)}
                       {:key :x :action (act.EmitEvent :ReloadFixup)}]
      :control_mode [{:key :Escape :action act.PopKeyTable}
                     {:key :g :action (act.EmitEvent :ActivateGitui)}
                     {:key :j :action (act.EmitEvent :ActivateLazyjj)}
                     {:key :s :action (act.EmitEvent :ActivateSerpl)}
                     {:key :n :action (act.EmitEvent :NewProjectWindow)}]})

(for [i 1 9]
  (table.insert config.key_tables.tab_mode
                {:key (tostring i) :action (act.ActivateTab (- i 1))}))

(set config.keys
     [{:key :c
       :mods :LEADER
       :action (act.ActivateKeyTable {:name :control_mode
                                      :one_shot false
                                      :until_unknown true
                                      :replace_current true})}
      {:key :p
       :mods :LEADER
       :action (act.ActivateKeyTable {:name :pane_mode
                                      :one_shot false
                                      :until_unknown true
                                      :replace_current true})}
      {:key :w
       :mods :LEADER
       :action (act.ActivateKeyTable {:name :workspace_mode
                                      :one_shot false
                                      :until_unknown true
                                      :replace_current true})}
      {:key :t
       :mods :LEADER
       :action (act.ActivateKeyTable {:name :tab_mode
                                      :one_shot false
                                      :until_unknown true
                                      :replace_current true})}])

(set config.unix_domains [{:name :local-dev}])
(set config.ssh_domains (load-ssh-domains))

config
