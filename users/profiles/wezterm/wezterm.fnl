(local wezterm _G.wezterm)
(local act wezterm.action)
(local mux wezterm.mux)
(local config (wezterm.config_builder))

(local dev-remote _G.dev_remote)
(local project-dir :/home/john/Development)
(local project-dirname (string.gsub project-dir "(.*/)(.*)" "%2"))
(local project-dir-find-cmd
       (wezterm.shell_split (.. "fd \\.git$ " project-dir
                                " -d 3 -H -t d -x echo {//}")))

(lambda table-concat [t1 t2]
  "Concatenate two tables"
  (var tc [])
  (each [i n (ipairs t1)] (table.insert tc n))
  (each [i n (ipairs t2)] (table.insert tc n))
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
  (wezterm.log_info "table-find, tbl: " tbl)
  (each [name value (pairs tbl) &until item]
    (when (cmp name value) (set item value)))
  (wezterm.log_info "table-find, returns: " item)
  item)

(lambda project-name [path]
  "Get the project/workspace name for the given path"
  (if (has-prefix path "/")
      (let [name (string.gsub path "(.*/)(.*)" "%2")
            dirname-path (string.gsub path "(.*)/(.*)" "%1")
            dirname (string.gsub dirname-path "(.*/)(.*)" "%2")]
        (if (not= dirname project-dirname) (.. dirname "/" name) name))
      path))

(lambda run-child-process [window pane args]
  "Runs a child process in the given context (i.e ssh if an ssh domain)"
  (let [domain (pane:get_domain_name)
        args (accumulate [a args _ d (pairs config.ssh_domains)
                          &until (= :ssh (. a 1))]
               (if (= d domain)
                   [:ssh (.. d.username "@" d.remote_address) (table.unpack a)]
                   a))
        (status out err) (wezterm.run_child_process args)]
    (values status out err)))

(lambda spawn-project-window [window pane]
  "Spawn a new window in the current project/workspace and domain"
  (let [domain (pane:get_domain_name)
        cwd (pane:get_current_working_dir)]
    (mux.spawn_window {:domain {:DomainName domain} :cwd cwd.file_path})))

(lambda project-directory-list [window pane]
  "Returns a list of paths to projects"
  (let [(status out err) (run-child-process window pane project-dir-find-cmd)]
    (var listing [])
    ;\r
    (each [line (out:gmatch "[^\n]+")]
      (table.insert listing line))
    listing))

(lambda project-workspace-config [window pane dir]
  "Reads and returns the workspace config for a project"
  (let [(status out err) (run-child-process window pane
                                            [:cat (.. dir :/workspace.yaml)])]
    (if status (wezterm.serde.yaml_decode out) {:windows [{}]})))

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
  (var args [])
  (var cmd "")
  (when window-or-pane.command
    (do
      (table.insert args :bash)
      (table.insert args :-c)
      (when (or (= window-or-pane.exit_to_shell nil)
                (= window-or-pane.exit_to_shell true))
        (set cmd (.. cmd "trap \"exec $SHELL\" SIGINT; ")))
      (if window-or-pane.restart
          (set cmd (.. "while true; do " window-or-pane.command
                       "; sleep 1; done"))
          (set cmd (.. cmd window-or-pane.command)))
      (table.insert args cmd)))
  args)

(local workspaces {})
(lambda setup-project-workspace [window pane name directory]
  (var meta {:panes {}})
  (let [domain (pane:get_domain_name)]
    (if (not (has-workspace name))
        (do
          (set (. workspaces name) meta)
          (let [workspace-config (project-workspace-config window pane
                                                           directory)]
            (each [_ window-conf (reverse-ipairs workspace-config.windows)]
              (let [args (command-for window-conf)
                    (tab pane window) (mux.spawn_window {:domain {:DomainName domain}
                                                         :workspace name
                                                         :cwd directory
                                                         : args})]
                (do
                  (tab:set_title :main)
                  (wezterm.time.call_after 0.5
                                           (fn []
                                             (print "args: " args)
                                             (print "panes: " window-conf.panes)
                                             (var panes [pane])
                                             (set (. meta :panes
                                                     window-conf.name)
                                                  {:pane_id (pane:pane_id)})
                                             (when window-conf.panes
                                               (print "panes: "
                                                      window-conf.panes)
                                               (each [_ pane-conf (ipairs window-conf.panes)]
                                                 (let [args (command-for pane-conf)
                                                       direction (or pane-conf.direction
                                                                     :Right)
                                                       size (or pane-conf.size
                                                                0.5)]
                                                   (do
                                                     (print "pane-conf: "
                                                            pane-conf)
                                                     (print "args: " args
                                                            " direction: "
                                                            direction " size: "
                                                            size)
                                                     (let [from-pane (. panes
                                                                        (or pane-conf.split_from
                                                                            (length panes)))
                                                           new-pane (from-pane:split {:cwd directory
                                                                                      :domain {:DomainName domain}
                                                                                      :workspace name
                                                                                      : direction
                                                                                      : size
                                                                                      : args})]
                                                       (set (. meta :panes
                                                               pane-conf.name)
                                                            {:pane_id (new-pane:pane_id)})
                                                       (table.insert panes
                                                                     new-pane))))))
                                             (-> (. panes 1) (: :activate)))))))))))
  (wezterm.log_info "set active ws: " name)
  (mux.set_active_workspace name))

(lambda reload-workspace-action [window pane]
  (let [name (window:active_workspace)
        directory (pane:get_current_working_dir)]
    (do
      (window:perform_action (act.CloseCurrentTab {:confirm false}) pane)
      (let [window (. (wezterm.gui.gui_windows) 1)
            pane (window:active_pane)]
        (setup-project-workspace window pane name directory.file_path)))))

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

(lambda open-gitui-action [window pane]
  (let [domain (pane:get_domain_name)
        cwd (pane:get_current_working_dir)]
    (wezterm.mux.spawn_window {:cwd cwd.file_path
                               :domain {:DomainName domain}
                               :args [:gitui]})))

(lambda open-named-tab-action [window pane name]
  (each [_ muxtab (ipairs (-> (window:mux_window) (: :tabs)))]
    (when (= (muxtab:get_title) name)
      (muxtab:activate))))

(lambda open-context-tab-action [window pane name args]
  (wezterm.log_info "activate " name " tab action")
  (if (table-contains (-> (window:mux_window) (: :tabs))
                      (fn [tab] (print "tabname: " (tab:get_title))
                        (= (tab:get_title) name)))
      (do
        (if (= (-> (window:active_tab) (: :get_title)) name)
            (open-named-tab-action window pane :main)
            (open-named-tab-action window pane name)))
      (let [domain (pane:get_domain_name)
            cwd (pane:get_current_working_dir)
            (tab pane window) (-> (window:mux_window)
                                  (: :spawn_tab
                                     {:cwd cwd.file_path
                                      :domain {:DomainName domain}
                                      : args}))]
        (tab:set_title name))))

(lambda pane-name-for-id [window pane-id]
  (wezterm.log_info window pane-id)
  (var pane-name (.. :pane- (tostring pane-id)))
  (let [ws (window:active_workspace)]
    (do
      (wezterm.log_info "ws: " ws " workspaces: " workspaces)
      (when (?. workspaces ws :panes)
        (each [name conf (pairs (. workspaces ws :panes))]
          (when (= conf.pane_id pane-id) (set pane-name name))))))
  (wezterm.log_info "pane-name: " pane-name)
  pane-name)

(lambda toggle-maximized-pane [pane-name]
  (lambda [window pane]
    (let [ws (window:active_workspace)
          tab (pane:tab)
          pane-id (. workspaces ws :panes pane-name :pane_id)
          zoomed (?. workspaces ws :panes pane-name :zoomed)]
      (do
        (wezterm.log_info "ws: " ws " tab: " tab " pane-id: " pane-id
                          " zoomed: " zoomed)
        (each [_ p (pairs (. workspaces ws :panes))]
          (set (. p :zoomed) false))
        (set (. workspaces ws :panes pane-name :zoomed) (not zoomed))
        (each [_ p (ipairs (tab:panes))]
          (when (= (p:pane_id) pane-id) (p:activate)
            (tab:set_zoomed (not zoomed))))))))

(lambda open-main-tab-action [window pane]
  (open-named-tab-action window pane :main))

(lambda open-gitui-tab-action [window pane]
  (open-context-tab-action window pane :gitui [:gitui]))

(lambda open-term-tab-action [window pane]
  (wezterm.log_info "workspaces: " workspaces)
  (open-context-tab-action window pane :term []))

; (lambda open-gitui-tab-action [window pane]
;   (wezterm.log_info "activate gitui tab action") ; (when (= (-> (window:active_tab) (: :get_title)) :gitui) ;   (each [_ muxtab (ipairs (-> (window:mux_window) (: :tabs)))] ;     (when (= (muxtab:get_title) :main) (muxtab:activate))))
;   (if (table-contains (-> (window:mux_window) (: :tabs))
;                       (fn [tabname] (= tabname :gitui)))
;       (do
;         (print "has gitui tab")
;         (each [_ muxtab (ipairs (-> (window:mux_window) (: :tabs)))]
;           (when (= (muxtab:get_title) :gitui) (muxtab:activate))))
;       (let [domain (pane:get_domain_name)
;             cwd (pane:get_current_working_dir)
;             (tab pane window) (-> (window:mux_window)
;                                   (: :spawn_tab
;                                      {:cwd cwd.file_path
;                                       :domain {:DomainName domain}
;                                       :args [:gitui]}))]
;         (tab:set_title :gitui))))

; (lambda open-term-tab-action [window pane]
;   (wezterm.log_info "activate term tab action")
;   (if (table-contains (-> (window:mux_window) (: :tabs))
;                       (fn [tab] (print "tabname: " (tab:get_title))
;                         (= (tab:get_title) :term)))
;       (do
;         (if (= (-> (window:active_tab) (: :get_title)) :term)
;             (each [_ muxtab (ipairs (-> (window:mux_window) (: :tabs)))]
;               (when (= (muxtab:get_title) :main)
;                 (print "activate main")
;                 (muxtab:activate)))
;             (each [_ muxtab (ipairs (-> (window:mux_window) (: :tabs)))]
;               (when (= (muxtab:get_title) :term)
;                 (print "activate term")
;                 (muxtab:activate)))))
;       (let [domain (pane:get_domain_name)
;             cwd (pane:get_current_working_dir)
;             (tab pane window) (-> (window:mux_window)
;                                   (: :spawn_tab
;                                      {:cwd cwd.file_path
;                                       :domain {:DomainName domain}}))]
;         (tab:set_title :term))))

; (lambda open-gitui-tab-action [window pane]
;   (wezterm.log_info "activate gitui tab action")
;   (let [domain (pane:get_domain_name)
;         cwd (pane:get_current_working_dir)
;         (tab pane window) (-> (window:mux_window)
;                               (: :spawn_tab
;                                  {: cwd
;                                   :domain {:DomainName domain}
;                                   :args [:gitui]}))]
;     (tab:set_title :gitui)))

(wezterm.on :ReloadWorkspace reload-workspace-action)
(wezterm.on :ActivateContextUI open-gitui-tab-action)
(wezterm.on :ActivateUtilUI (toggle-maximized-pane :term))
(wezterm.on :ToggleMaximizeEditor (toggle-maximized-pane :editor))
(wezterm.on :ActivateMainUI open-main-tab-action)
(wezterm.on :ReloadFixup
            (lambda [window pane]
              (run-child-process window pane [:pkill :-HUP :direnv])))

(wezterm.on :FindProject open-select-project-window)
(wezterm.on :NewProjectWindow spawn-project-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tab bar styling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The filled in variant of the < symbol
(local solid-left-arrow wezterm.nerdfonts.pl_right_hard_divider)

; The filled in variant of the > symbol
(local solid-right-arrow wezterm.nerdfonts.pl_left_hard_divider)

(local remote-dev-color "#1e1e2e")
(local local-dev-color "#ae4d1a")
(local local-term-color "#22336e")

(local text-fg "#c0c0c0")

(wezterm.on :update-status
            (lambda [window pane]
              (var tab-bg local-term-color)
              (let [domain (pane:get_domain_name)
                    pane-info (table-find (-> (window:active_tab)
                                              (: :panes_with_info))
                                          (fn [_ value]
                                            (= (value.pane:pane_id)
                                               (pane:pane_id))))
                    overrides (or (window:get_config_overrides) {})
                    cwd-uri (pane:get_current_working_dir)
                    (_ hostname _) (run-child-process window pane [:hostname])
                    ws (window:active_workspace)]
                (case domain
                  :remote-dev (set tab-bg remote-dev-color)
                  :local-dev (set tab-bg local-dev-color))
                (set overrides.colors {:tab_bar {:background tab-bg}})
                (window:set_config_overrides overrides)
                (let [colors [tab-bg
                              "#51576d"
                              "#838ba7"
                              "#7287fd"
                              "#04a5e5"
                              "#04a5e5"
                              "#7287fd"]
                      cells [""
                             (.. domain "/" hostname)
                             ws
                             (pane-name-for-id window (pane:pane_id))
                             (if pane-info.is_zoomed " 👁 " "  ")]
                      elements (accumulate [elements [] i cell (reverse-ipairs cells)]
                                 (do
                                   (table.insert elements
                                                 {:Foreground {:Color text-fg}})
                                   (table.insert elements
                                                 {:Background {:Color (. colors
                                                                         i)}})
                                   (table.insert elements
                                                 {:Text (.. " " cell " ")})
                                   (when (not= i (length cells))
                                     (do
                                       (table.insert elements
                                                     {:Foreground {:Color (. colors
                                                                             (+ i
                                                                                1))}})
                                       (table.insert elements
                                                     {:Text solid-left-arrow})))
                                   elements))]
                  (window:set_right_status (wezterm.format elements))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set config.mux_env_remove {})
(set config.adjust_window_size_when_changing_font_size false)
(set config.enable_wayland true)
(set config.enable_tab_bar true)
(set config.use_fancy_tab_bar false)
(set config.tab_max_width 64)
(set config.show_tabs_in_tab_bar false)
(set config.show_new_tab_button_in_tab_bar false)
(set config.tab_bar_at_bottom true)
(set config.hide_tab_bar_if_only_one_tab false)
(set config.font (wezterm.font "JetBrainsMono Nerd Font"))
(set config.font_size 14)
(set config.color_scheme "Nord (Gogh)")
(set config.window_background_opacity 0.91)

(set config.leader {:key :Space :mods :CTRL})
(set config.keys
     [{:key :Space
       :mods :LEADER|CTRL
       :action (act.SendKey {:key :Space :mods :CTRL})}
      {:key :g :mods :CTRL :action (act.EmitEvent :ActivateContextUI)}
      {:key :t :mods :CTRL :action (act.EmitEvent :ActivateUtilUI)}
      {:key :e :mods :CTRL :action (act.EmitEvent :ToggleMaximizeEditor)}
      {:key :m :mods :CTRL :action (act.EmitEvent :ActivateMainUI)}
      {:key :n :mods :LEADER :action (act.EmitEvent :NewProjectWindow)}
      {:key :r :mods :LEADER :action (act.EmitEvent :ReloadFixup)}
      {:key :w :mods :CTRL|SHIFT :action (act.EmitEvent :ReloadWorkspace)}
      {:key :q :mods :LEADER :action (act.CloseCurrentPane {:confirm true})}
      {:key :g :mods :LEADER :action act.ShowTabNavigator}
      {:key :z :mods :CTRL :action act.TogglePaneZoomState}
      {:key :LeftArrow :mods :CTRL :action (act.ActivatePaneDirection :Left)}
      {:key :RightArrow :mods :CTRL :action (act.ActivatePaneDirection :Right)}
      {:key :UpArrow :mods :CTRL :action (act.ActivatePaneDirection :Up)}
      {:key :DownArrow :mods :CTRL :action (act.ActivatePaneDirection :Down)}
      {:key :q
       :mods :LEADER|SHIFT
       :action (act.CloseCurrentTab {:confirm true})}
      {:key :r :mods :CTRL :action (act.RotatePanes :CounterClockwise)}
      {:key :r :mods :CTRL|SHIFT :action (act.RotatePanes :Clockwise)}
      {:key :p :mods :CTRL :action (act.PaneSelect {:mode :SwapWithActive})}
      {:key :a :mods :LEADER :action (act.EmitEvent :FindProject)}
      {:key :f
       :mods :LEADER
       :action (act.ShowLauncherArgs {:flags :FUZZY|WORKSPACES})}])

(set config.unix_domains [{:name :local-dev}])
(set config.ssh_domains [{:name :remote-dev
                          :remote_address dev-remote
                          :username :john}])

config

