{pkgs, ...}: {
  xdg.cacheFile."zellij/permissions.kdl".force = true;
  xdg.cacheFile."zellij/permissions.kdl".text = ''
    "${pkgs.zjstatus}/bin/zjstatus.wasm" {
          RunCommands
          ChangeApplicationState
          ReadApplicationState
      }
      "${pkgs.zjstatus-hints}/bin/zjstatus-hints.wasm" {
          ReadApplicationState
          MessageAndLaunchOtherPlugins
      }
      "${pkgs.zwift}/bin/zwift.wasm" {
          ReadApplicationState
          ChangeApplicationState
      }
  '';
  xdg.configFile."zellij/layouts/default.kdl".text = ''
    layout {
      default_tab_template {
          children
          pane size=1 borderless=true {
              plugin location="file:${pkgs.zjstatus}/bin/zjstatus.wasm" {
                format_left   "{mode} {tabs}"
                format_center ""
                format_right  "{pipe_zjstatus_hints}#[fg=#8bd5ca]#[bg=#8bd5ca,fg=#1e2030,bold]{command_hostname}#[bg=#89b4fa,fg=#203040,bold] {session}#[fg=#89b4fa]"
                format_space  " "
                format_hide_on_overlength "true"
                format_precedence "crl"

                pipe_zjstatus_hints_format "{output}"

                border_enabled  "false"
                border_char     "─"
                border_format   "#[fg=#6C7086]{char}"
                border_position "top"

                mode_normal      "#[fg=#a6d39f]#[bg=#a6d39f,fg=#203040,bold] 💠 NORMAL#[fg=#a6d39f]"
                mode_locked      "#[fg=#6e738d]#[bg=#6e738d,fg=#203040,bold] 🔒 LOCKED#[fg=#6e738d]"
                mode_resize      "#[fg=#8aadf4]#[bg=#8aadf4,fg=#203040,bold] 📏 RESIZE#[fg=#8aadf4]"
                mode_pane        "#[fg=#a6e3a1]#[bg=#a6e3a1,fg=#203040,bold] 🪟 PANE#[fg=#a6e3a1]"
                mode_move        "#[fg=#f9e2af]#[bg=#f9e2af,fg=#203040,bold] ✋ MOVE#[fg=#f9e2af]"
                mode_tab         "#[fg=#89b4fa]#[bg=#89b4fa,fg=#203040,bold] 📑 TAB#[fg=#89b4fa]"
                mode_scroll      "#[fg=#cba6f7]#[bg=#cba6f7,fg=#203040,bold] 🌀 SCROLL#[fg=#cba6f7]"
                mode_search      "#[fg=#f9e2af]#[bg=#f9e2af,fg=#203040,bold] 🔍 SEARCH#[fg=#f9e2af]"
                mode_entersearch "#[fg=#f9e2af]#[bg=#f9e2af,fg=#203040,bold] 🧭 ENTER SEARCH#[fg=#f9e2af]"
                mode_renametab   "#[fg=#89b4fa]#[bg=#89b4fa,fg=#203040,bold] ✎ RENAME TAB#[fg=#89b4fa]"
                mode_renamepane  "#[fg=#a6e3a1]#[bg=#a6e3a1,fg=#203040,bold] ✎ RENAME PANE#[fg=#a6e3a1]"
                mode_session     "#[fg=#f38ba8]#[bg=#f38ba8,fg=#203040,bold] 💻 SESSION#[fg=#f38ba8]"
                mode_tmux        "#[fg=#cba6f7]#[bg=#cba6f7,fg=#203040,bold] 🧩 TMUX#[fg=#cba6f7]"


                tab_normal              "#[fg=#6c7086]#[bg=#6c7086,fg=#1e2030,bold]{index} #[bg=#585b70,fg=#cdd6f4,bold] {name}{floating_indicator}#[fg=#585b70]"
                tab_normal_fullscreen   "#[fg=#6c7086]#[bg=#6c7086,fg=#1e2030,bold]{index} #[bg=#585b70,fg=#cdd6f4,bold] {name}{fullscreen_indicator}#[fg=#585b70]"
                tab_normal_sync         "#[fg=#6c7086]#[bg=#6c7086,fg=#1e2030,bold]{index} #[bg=#585b70,fg=#cdd6f4,bold] {name}{sync_indicator}#[fg=#585b70]"

                tab_active              "#[fg=#eed49f]#[bg=#eed49f,fg=#1e2030,bold]{index} #[bg=#89b4fa,fg=#203040,bold] {name}{floating_indicator}#[fg=#89b4fa]"
                tab_active_fullscreen   "#[fg=#eed49f]#[bg=#eed49f,fg=#1e2030,bold]{index} #[bg=#89b4fa,fg=#203040,bold] {name}{fullscreen_indicator}#[fg=#89b4fa]"
                tab_active_sync         "#[fg=#eed49f]#[bg=#eed49f,fg=#1e2030,bold]{index} #[bg=#89b4fa,fg=#203040,bold] {name}{sync_indicator}#[fg=#89b4fa]"

                tab_rename              "#[fg=#eed49f]#[bg=#eed49f,fg=#1e2030,bold]{index} #[bg=#89b4fa,fg=#203040,bold] {name}{floating_indicator}#[fg=#89b4fa]"

                tab_display_count         "9"
                tab_truncate_start_format "#[fg=#f9e2af] <U+F0D9> +{count} <U+EA7C> "
                tab_truncate_end_format   "#[fg=#f9e2af] <U+EA7C>  +{count} <U+F0DA>"

                tab_separator " "

                tab_sync_indicator       " ⟳"
                tab_fullscreen_indicator " ◉"
                tab_floating_indicator   " ▣"

                command_hostname_command     "hostname"
                command_hostname_format      "#[bg=#8bd5ca,fg=#1e2030,bold]{stdout} "
                command_hostname_interval    "0"
                command_hostname_rendermode  "static"
              }
          }
      }
    }
  '';
  xdg.configFile."zellij/config.kdl".text = ''
    keybinds clear-defaults=true {
        locked {
            bind "Ctrl g" { SwitchToMode "Normal"; }
        }
        resize {
            bind "Ctrl n" { SwitchToMode "Normal"; }
            bind "h" "Left" { Resize "Increase Left"; }
            bind "j" "Down" { Resize "Increase Down"; }
            bind "k" "Up" { Resize "Increase Up"; }
            bind "l" "Right" { Resize "Increase Right"; }
            bind "H" { Resize "Decrease Left"; }
            bind "J" { Resize "Decrease Down"; }
            bind "K" { Resize "Decrease Up"; }
            bind "L" { Resize "Decrease Right"; }
            bind "=" "+" { Resize "Increase"; }
            bind "-" { Resize "Decrease"; }
        }
        pane {
            bind "Ctrl p" { SwitchToMode "Normal"; }
            bind "h" "Left" { MoveFocus "Left"; }
            bind "l" "Right" { MoveFocus "Right"; }
            bind "j" "Down" { MoveFocus "Down"; }
            bind "k" "Up" { MoveFocus "Up"; }
            bind "p" { SwitchFocus; }
            bind "n" { NewPane; SwitchToMode "Normal"; }
            bind "d" { NewPane "Down"; SwitchToMode "Normal"; }
            bind "r" { NewPane "Right"; SwitchToMode "Normal"; }
            bind "s" { NewPane "stacked"; SwitchToMode "Normal"; }
            bind "x" { CloseFocus; SwitchToMode "Normal"; }
            bind "f" { ToggleFocusFullscreen; SwitchToMode "Normal"; }
            bind "z" { TogglePaneFrames; SwitchToMode "Normal"; }
            bind "w" { ToggleFloatingPanes; SwitchToMode "Normal"; }
            bind "e" { TogglePaneEmbedOrFloating; SwitchToMode "Normal"; }
            bind "c" { SwitchToMode "RenamePane"; PaneNameInput 0;}
            bind "i" { TogglePanePinned; SwitchToMode "Normal"; }
        }
        move {
            bind "Ctrl h" { SwitchToMode "Normal"; }
            bind "n" "Tab" { MovePane; }
            bind "p" { MovePaneBackwards; }
            bind "h" "Left" { MovePane "Left"; }
            bind "j" "Down" { MovePane "Down"; }
            bind "k" "Up" { MovePane "Up"; }
            bind "l" "Right" { MovePane "Right"; }
        }
        tab {
            bind "Ctrl t" { SwitchToMode "Normal"; }
            bind "r" { SwitchToMode "RenameTab"; TabNameInput 0; }
            bind "h" "Left" "Up" "k" { GoToPreviousTab; }
            bind "l" "Right" "Down" "j" { GoToNextTab; }
            bind "n" { NewTab; SwitchToMode "Normal"; }
            bind "x" { CloseTab; SwitchToMode "Normal"; }
            bind "s" { ToggleActiveSyncTab; SwitchToMode "Normal"; }
            bind "b" { BreakPane; SwitchToMode "Normal"; }
            bind "]" { BreakPaneRight; SwitchToMode "Normal"; }
            bind "[" { BreakPaneLeft; SwitchToMode "Normal"; }
            bind "1" { GoToTab 1; SwitchToMode "Normal"; }
            bind "2" { GoToTab 2; SwitchToMode "Normal"; }
            bind "3" { GoToTab 3; SwitchToMode "Normal"; }
            bind "4" { GoToTab 4; SwitchToMode "Normal"; }
            bind "5" { GoToTab 5; SwitchToMode "Normal"; }
            bind "6" { GoToTab 6; SwitchToMode "Normal"; }
            bind "7" { GoToTab 7; SwitchToMode "Normal"; }
            bind "8" { GoToTab 8; SwitchToMode "Normal"; }
            bind "9" { GoToTab 9; SwitchToMode "Normal"; }
            bind "Tab" { ToggleTab; }
        }
        scroll {
            bind "Ctrl s" { SwitchToMode "Normal"; }
            bind "e" { EditScrollback; SwitchToMode "Normal"; }
            bind "s" { SwitchToMode "EnterSearch"; SearchInput 0; }
            bind "Ctrl c" { ScrollToBottom; SwitchToMode "Normal"; }
            bind "j" "Down" { ScrollDown; }
            bind "k" "Up" { ScrollUp; }
            bind "Ctrl f" "PageDown" "Right" "l" { PageScrollDown; }
            bind "Ctrl b" "PageUp" "Left" "h" { PageScrollUp; }
            bind "d" { HalfPageScrollDown; }
            bind "u" { HalfPageScrollUp; }
        }
        search {
            bind "Ctrl s" { SwitchToMode "Normal"; }
            bind "Ctrl c" { ScrollToBottom; SwitchToMode "Normal"; }
            bind "j" "Down" { ScrollDown; }
            bind "k" "Up" { ScrollUp; }
            bind "Ctrl f" "PageDown" "Right" "l" { PageScrollDown; }
            bind "Ctrl b" "PageUp" "Left" "h" { PageScrollUp; }
            bind "d" { HalfPageScrollDown; }
            bind "u" { HalfPageScrollUp; }
            bind "n" { Search "down"; }
            bind "p" { Search "up"; }
            bind "c" { SearchToggleOption "CaseSensitivity"; }
            bind "w" { SearchToggleOption "Wrap"; }
            bind "o" { SearchToggleOption "WholeWord"; }
        }
        entersearch {
            bind "Ctrl c" "Esc" { SwitchToMode "Scroll"; }
            bind "Enter" { SwitchToMode "Search"; }
        }
        renametab {
            bind "Ctrl c" { SwitchToMode "Normal"; }
            bind "Esc" { UndoRenameTab; SwitchToMode "Tab"; }
        }
        renamepane {
            bind "Ctrl c" { SwitchToMode "Normal"; }
            bind "Esc" { UndoRenamePane; SwitchToMode "Pane"; }
        }
        session {
            bind "Ctrl o" { SwitchToMode "Normal"; }
            bind "Ctrl s" { SwitchToMode "Scroll"; }
            bind "d" { Detach; }
            bind "w" {
                LaunchOrFocusPlugin "session-manager" {
                    floating true
                    move_to_focused_tab true
                };
                SwitchToMode "Normal"
            }
            bind "c" {
                LaunchOrFocusPlugin "configuration" {
                    floating true
                    move_to_focused_tab true
                };
                SwitchToMode "Normal"
            }
            bind "p" {
                LaunchOrFocusPlugin "plugin-manager" {
                    floating true
                    move_to_focused_tab true
                };
                SwitchToMode "Normal"
            }
            bind "a" {
                LaunchOrFocusPlugin "zellij:about" {
                    floating true
                    move_to_focused_tab true
                };
                SwitchToMode "Normal"
            }
            bind "s" {
                LaunchOrFocusPlugin "zellij:share" {
                    floating true
                    move_to_focused_tab true
                };
                SwitchToMode "Normal"
            }
        }
        shared_except "locked" {
            bind "Ctrl g" { SwitchToMode "Locked"; }
            bind "Ctrl q" { Quit; }
            bind "Alt f" { ToggleFloatingPanes; }
            bind "Alt n" { NewPane; }
            bind "Alt i" { MoveTab "Left"; }
            bind "Alt o" { MoveTab "Right"; }
            bind "Alt h" "Alt Left" { MoveFocusOrTab "Left"; }
            bind "Alt l" "Alt Right" { MoveFocusOrTab "Right"; }
            bind "Alt j" "Alt Down" { MoveFocus "Down"; }
            bind "Alt k" "Alt Up" { MoveFocus "Up"; }
            bind "Alt =" "Alt +" { Resize "Increase"; }
            bind "Alt -" { Resize "Decrease"; }
            bind "Alt [" { PreviousSwapLayout; }
            bind "Alt ]" { NextSwapLayout; }
            bind "Alt p" { TogglePaneInGroup; }
            bind "Alt Shift p" { ToggleGroupMarking; }
        }
        shared_except "normal" "locked" {
            bind "Enter" "Esc" { SwitchToMode "Normal"; }
        }
        shared_except "pane" "locked" {
            bind "Ctrl p" { SwitchToMode "Pane"; }
        }
        shared_except "resize" "locked" {
            bind "Ctrl n" { SwitchToMode "Resize"; }
        }
        shared_except "scroll" "locked" {
            bind "Ctrl s" { SwitchToMode "Scroll"; }
        }
        shared_except "session" "locked" {
            bind "Ctrl o" { SwitchToMode "Session"; }
        }
        shared_except "tab" "locked" {
            bind "Ctrl t" { SwitchToMode "Tab"; }
        }
        shared_except "move" "locked" {
            bind "Ctrl h" { SwitchToMode "Move"; }
        }
    }

    // Plugin aliases - can be used to change the implementation of Zellij
    // changing these requires a restart to take effect
    plugins {
        tab-bar location="zellij:tab-bar"
        status-bar location="zellij:status-bar"
        strider location="zellij:strider"
        compact-bar location="zellij:compact-bar"
        session-manager location="zellij:session-manager"
        welcome-screen location="zellij:session-manager" {
            welcome_screen true
        }
        filepicker location="zellij:strider" {
            cwd "/"
        }
        configuration location="zellij:configuration"
        plugin-manager location="zellij:plugin-manager"
        about location="zellij:about"
        zjstatus-hints location="file:${pkgs.zjstatus-hints}/bin/zjstatus-hints.wasm" {
            // Maximum number of characters to display
            max_length 140 // 0 = unlimited
            // String to append when truncated
            overflow_str "..." // default
            // Name of the pipe for zjstatus integration
            pipe_name "zjstatus_hints" // default
            // Hide hints in base mode (a.k.a. default mode)
            // E.g. if you have set default_mode to "locked", then
            // you can hide hints in the locked mode by setting this to true
            hide_in_base_mode true // default
        }
    }

    // Plugins to load in the background when a new session starts
    load_plugins {
        zjstatus-hints
      // "file:/path/to/my-plugin.wasm"
      // "https://example.com/my-plugin.wasm"
    }

    // Choose what to do when zellij receives SIGTERM, SIGINT, SIGQUIT or SIGHUP
    // eg. when terminal window with an active zellij session is closed
    // (Requires restart)
    // Options:
    //   - detach (Default)
    //   - quit
    //
    // on_force_close "quit"

    //  Send a request for a simplified ui (without arrow fonts) to plugins
    //  Options:
    //    - true
    //    - false (Default)
    //
    // simplified_ui true

    // Choose the path to the default shell that zellij will use for opening new panes
    // Default: $SHELL
    //
    default_shell "nu"

    // Choose the path to override cwd that zellij will use for opening new panes
    //
    // default_cwd ""

    // Toggle between having pane frames around the panes
    // Options:
    //   - true (default)
    //   - false
    //
    pane_frames false

    // Toggle between having Zellij lay out panes according to a predefined set of layouts whenever possible
    // Options:
    //   - true (default)
    //   - false
    //
    // auto_layout true

    // Whether sessions should be serialized to the cache folder (including their tabs/panes, cwds and running commands) so that they can later be resurrected
    // (Requires restart)
    // Options:
    //   - true (default)
    //   - false
    //
    // session_serialization false

    // Whether pane viewports are serialized along with the session, default is false
    // (Requires restart)
    // Options:
    //   - true
    //   - false (default)
    //
    // serialize_pane_viewport true

    // Scrollback lines to serialize along with the pane viewport when serializing sessions, 0
    // defaults to the scrollback size. If this number is higher than the scrollback size, it will
    // also default to the scrollback size. This does nothing if `serialize_pane_viewport` is not true.
    // (Requires restart)
    //
    // scrollback_lines_to_serialize 10000

    // Define color themes for Zellij
    // For more examples, see: https://github.com/zellij-org/zellij/tree/main/example/themes
    // Once these themes are defined, one of them should to be selected in the "theme" section of this file
    //
    // themes {
    //     dracula {
    //         fg 248 248 242
    //         bg 40 42 54
    //         red 255 85 85
    //         green 80 250 123
    //         yellow 241 250 140
    //         blue 98 114 164
    //         magenta 255 121 198
    //         orange 255 184 108
    //         cyan 139 233 253
    //         black 0 0 0
    //         white 255 255 255
    //     }
    // }

    // Choose the theme that is specified in the themes section.
    // Default: default
    //
    theme "nord"

    // The name of the default layout to load on startup
    // Default: "default"
    // (Requires restart)
    //
    // default_layout "compact"

    // Choose the mode that zellij uses when starting up.
    // Default: normal
    //
    // default_mode "locked"

    // Toggle enabling the mouse mode.
    // On certain configurations, or terminals this could
    // potentially interfere with copying text.
    // (Requires restart)
    // Options:
    //   - true (default)
    //   - false
    //
    // mouse_mode false

    // Configure the scroll back buffer size
    // This is the number of lines zellij stores for each pane in the scroll back
    // buffer. Excess number of lines are discarded in a FIFO fashion.
    // (Requires restart)
    // Valid values: positive integers
    // Default value: 10000
    //
    // scroll_buffer_size 10000

    // Provide a command to execute when copying text. The text will be piped to
    // the stdin of the program to perform the copy. This can be used with
    // terminal emulators which do not support the OSC 52 ANSI control sequence
    // that will be used by default if this option is not set.
    // Examples:
    //
    // copy_command "xclip -selection clipboard" // x11
    // copy_command "wl-copy"                    // wayland
    // copy_command "pbcopy"                     // osx

    // Choose the destination for copied text
    // Allows using the primary selection buffer (on x11/wayland) instead of the system clipboard.
    // Does not apply when using copy_command.
    // Options:
    //   - system (default)
    //   - primary
    //
    // copy_clipboard "primary"

    // Enable or disable automatic copy (and clear) of selection when releasing mouse
    // Default: true
    //
    // copy_on_select false

    // Path to the default editor to use to edit pane scrollbuffer
    // Default: $EDITOR or $VISUAL
    //
    // scrollback_editor "/usr/bin/vim"

    // When attaching to an existing session with other users,
    // should the session be mirrored (true)
    // or should each user have their own cursor (false)
    // (Requires restart)
    // Default: false
    //
    // mirror_session true

    // The folder in which Zellij will look for layouts
    // (Requires restart)
    //
    // layout_dir "/path/to/my/layout_dir"

    // The folder in which Zellij will look for themes
    // (Requires restart)
    //
    // theme_dir "/path/to/my/theme_dir"

    // Enable or disable the rendering of styled and colored underlines (undercurl).
    // May need to be disabled for certain unsupported terminals
    // (Requires restart)
    // Default: true
    //
    // styled_underlines false

    // Enable or disable writing of session metadata to disk (if disabled, other sessions might not know
    // metadata info on this session)
    // (Requires restart)
    // Default: false
    //
    // disable_session_metadata true

    // Enable or disable support for the enhanced Kitty Keyboard Protocol (the host terminal must also support it)
    // (Requires restart)
    // Default: true (if the host terminal supports it)
    //
    // support_kitty_keyboard_protocol false

    // Whether to make sure a local web server is running when a new Zellij session starts.
    // This web server will allow creating new sessions and attaching to existing ones that have
    // opted in to being shared in the browser.
    // When enabled, navigate to http://127.0.0.1:8082
    // (Requires restart)
    //
    // Note: a local web server can still be manually started from within a Zellij session or from the CLI.
    // If this is not desired, one can use a version of Zellij compiled without
    // `web_server_capability`
    //
    // Possible values:
    // - true
    // - false
    // Default: false
    //
    web_server true

    // Whether to allow sessions started in the terminal to be shared through a local web server, assuming one is
    // running (see the `web_server` option for more details).
    // (Requires restart)
    //
    // Note: This is an administrative separation and not intended as a security measure.
    //
    // Possible values:
    // - "on" (allow web sharing through the local web server if it
    // is online)
    // - "off" (do not allow web sharing unless sessions explicitly opt-in to it)
    // - "disabled" (do not allow web sharing and do not permit sessions started in the terminal to opt-in to it)
    // Default: "off"
    //
    web_sharing "on"

    // The ip address the web server should listen on when it starts
    // Default: "127.0.0.1"
    // (Requires restart)
    //
    web_server_ip "127.0.0.1"


    // A path to a certificate file to be used when setting up the web client to serve the
    // connection over HTTPs
    //
    // web_server_cert "/path/to/my/cert.pem"

    // A path to a key file to be used when setting up the web client to serve the
    // connection over HTTPs
    //
    // web_server_key "/path/to/my/key.pem"

    // Whether to enforce https connections to the web server when it is bound to localhost
    // (127.0.0.0/8)
    //
    // Note: https is ALWAYS enforced when bound to non-local interfaces
    //
    // Default: false
    //
    // enforce_https_for_localhost true

    // The port the web server should listen on when it starts
    // Default: 8082
    // (Requires restart)
    //
    // web_server_port 8082

    // Whether to stack panes when resizing beyond a certain size
    // Default: true
    //
    // stacked_resize false

    // Whether to show release notes on first version run
    // Default: true
    //
    // show_release_notes false

    // Whether to enable mouse hover effects and pane grouping functionality
    // Default: true
    //
    // advanced_mouse_actions false

    // A command to run (will be wrapped with sh -c and provided the RESURRECT_COMMAND env variable)
    // after Zellij attempts to discover a command inside a pane when resurrecting sessions, the STDOUT
    // of this command will be used instead of the discovered RESURRECT_COMMAND
    // can be useful for removing wrappers around commands
    // Note: be sure to escape backslashes and similar characters properly
    //
    // post_command_discovery_hook "echo $RESURRECT_COMMAND | sed <your_regex_here>"

    show_startup_tips false
    show_release_notes false
    web_client {
        font "JetBrainsMono Nerd Font"
    }

  '';
  programs.zellij = {
    enable = true;
  };
}
