{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.home) username;
  openProject = pkgs.writeShellApplication {
    name = "zellij-open-project";
    runtimeInputs = [pkgs.zellij pkgs.fd pkgs.skim];
    text = ''
      # shellcheck disable=SC1083
      project="$(fd \.git /home/john/Development -d 3 -u -t d -x echo {//} | sort -u | sk)"
      zellij pipe --name zwift_selection "$project"
    '';
  };
  openSession = pkgs.writeShellApplication {
    name = "zellij-open-session";
    runtimeInputs = [pkgs.zellij pkgs.skim];
    text = ''
      # shellcheck disable=SC1083
      session="$(zellij ls -s | sk)"
      zellij pipe --name zwift_selection "$session"
    '';
  };
  direnvExecMaybe = pkgs.writeShellApplication {
    name = "direnv-exec-maybe";
    runtimeInputs = [pkgs.direnv];
    text = ''
      if [ -f .envrc ]; then
        direnv exec . "$@"
      else
        "$@"
      fi
    '';
  };
  zjstatusPane = ''
    pane size=1 borderless=true {
      plugin location="file://${pkgs.zjstatus}/bin/zjstatus.wasm" {

        format_left  "{mode}#[fg=#89B4FA,bg=#181825,bold] {session}#[bg=#181825] {tabs}"
        format_center "{command_hostname}"
        format_right "{command_git_branch} {command_kubectx} {command_kubens} {datetime}"
        format_space "#[bg=#181825]"

        mode_normal          "#[bg=#89B4FA,fg=#000000] {name} "
        mode_tmux            "#[bg=#ffc387,fg=#000000] {name} "
        mode_default_to_mode "tmux"

        tab_normal               "#[fg=#6C7086,bg=#181825] {index} {name} {fullscreen_indicator}{sync_indicator}{floating_indicator}"
        tab_active               "#[fg=#9399B2,bg=#181825,bold,italic] {index} {name} {fullscreen_indicator}{sync_indicator}{floating_indicator}"
        tab_fullscreen_indicator "□ "
        tab_sync_indicator       "  "
        tab_floating_indicator   "󰉈 "

        command_kubectx_command  "kubectx -c"
        command_kubectx_format   "#[fg=#6C7086,bg=#181825,italic] {stdout}"
        command_kubectx_interval "2"

        command_kubens_command  "kubens -c"
        command_kubens_format   "#[fg=#6C7086,bg=#181825,bold]{stdout} "
        command_kubens_interval "2"

        command_hostname_command  "hostname"
        command_hostname_format   "#[fg=#6C7086,bg=#181825,bold]{stdout} "
        command_hostname_interval "30"

        command_git_branch_command     "git rev-parse --abbrev-ref HEAD"
        command_git_branch_format      "#[fg=#89B4FA,bg=#181825,bold] on {stdout} "
        command_git_branch_interval    "2"
        command_git_branch_rendermode  "static"

        datetime          "#[fg=#9399B2,bg=#181825] {format} "
        datetime_format   "%A, %d %b %Y %H:%M"
        datetime_timezone "Europe/Stockholm"
      }
    }
  '';
in {
  home.packages = [direnvExecMaybe];
  xdg.configFile."zellij/layouts/dev.kdl".text = ''

    layout {
      default_tab_template {
          children
          ${zjstatusPane}
      }

      tab_template name="main" {
        pane split_direction="vertical" {
          pane size="75%" command="direnv-exec-maybe" {
            args "hx" "."
          }
          pane split_direction="horizontal" stacked=true {
            pane
            pane
            pane
          }
        }

        floating_panes {
            pane {
              x "10%"
              y "2%"
              width "80%"
              height "80%"
            }
        }

        ${zjstatusPane}
      }

      main focus=true hide_floating_panes=true name="main"
    }
  '';
  xdg.configFile."zellij/layouts/default.kdl".text = ''
    layout {
      pane split_direction="horizontal" {
        pane
      }
      pane size=1 borderless=true {
        plugin location="zellij:compact-bar"
      }
    }
  '';
  xdg.configFile."zellij/config.kdl".text = ''
    theme "nord"
    pane_frames false
    session_serialization true
    serialization_interval 1

    keybinds clear-defaults=true {
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
            bind "x" { CloseFocus; SwitchToMode "Normal"; }
            bind "f" { ToggleFocusFullscreen; SwitchToMode "Normal"; }
            bind "z" { TogglePaneFrames; SwitchToMode "Normal"; }
            bind "w" { ToggleFloatingPanes; SwitchToMode "Normal"; }
            bind "e" { TogglePaneEmbedOrFloating; SwitchToMode "Normal"; }
            bind "c" { SwitchToMode "RenamePane"; PaneNameInput 0;}
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
            // uncomment this and adjust key if using copy_on_select=false
            // bind "Alt c" { Copy; }
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
                LaunchOrFocusPlugin "zellij:session-manager" {
                    floating true
                    move_to_focused_tab true
                };
                SwitchToMode "Normal"
            }
        }

        shared_except "locked" {
            bind "Ctrl g" { SwitchToMode "Locked"; }
            bind "Ctrl q" { Quit; }
            bind "Ctrl e" { SwitchToMode "Locked"; }
            bind "Ctrl g" {
              Run "${pkgs.gex}/bin/gex" {
                floating true
                close_on_exit true
              }
            }
            bind "Ctrl a" {
              LaunchOrFocusPlugin "file://${pkgs.zwift}/bin/zwift.wasm" {
                floating true
              }
              Run "${openProject}/bin/zellij-open-project" {
                cwd "/home/${username}"
                floating true
                close_on_exit true
              }
            }
            bind "Alt a" {
              LaunchOrFocusPlugin "file://${pkgs.zwift}/bin/zwift.wasm" {
                floating true
              }
              Run "${openSession}/bin/zellij-open-session" {
                cwd "/home/${username}"
                floating true
                close_on_exit true
              }
            }
            bind "Alt Left" { MoveFocusOrTab "Left"; }
            bind "Alt Right" { MoveFocusOrTab "Right"; }

            ${lib.concatStringsSep "\n" (builtins.genList (x: "bind \"Alt ${toString (x + 1)}\" { GoToTab ${toString (x + 1)}; }") 9)}

            bind "Alt Down" { MoveFocus "Down"; }
            bind "Alt Up" { MoveFocus "Up"; }
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
            bind "Ctrl s" { SwitchToMode "Session"; }
        }
        shared_except "tab" "locked" {
            bind "Ctrl t" { SwitchToMode "Tab"; }
        }
        shared_except "move" "locked" {
            bind "Ctrl h" { SwitchToMode "Move"; }
        }
    }

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
    }

  '';
  programs.zellij = {
    enable = true;
  };
}
