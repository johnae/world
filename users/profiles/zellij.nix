{
  pkgs,
  config,
  lib,
  ...
}: let
  inherit (config.home) username;
  openProject = pkgs.writeShellApplication {
    name = "zellij-open-project";
    runtimeInputs = [pkgs.zellij pkgs.fd pkgs.skim];
    text = ''
      # shellcheck disable=SC1083
      project="$(fd \.git /home/john/Development -d 3 -H -t d -x echo {//} | sort -u | sk)"
      name="$(basename "$project")"
      if zellij action query-tab-names | grep -q "$name"; then
        zellij action go-to-tab-name "$name"
        exit 0
      fi
      zellij action new-tab -l dev -c "$project"
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
in {
  xdg.configFile."zellij/layouts/dev.kdl".text = ''
    layout {
      pane split_direction="horizontal" {
        pane size="75%" command="${direnvExecMaybe}/bin/direnv-exec-maybe" {
          args "hx" "."
        }
        pane
      }
      pane size=1 borderless=true {
        plugin location="zellij:compact-bar"
      }
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
    keybinds {
      unbind "Ctrl b"
      unbind "Ctrl g"
      shared_except "locked" {
        bind "Ctrl e" { SwitchToMode "Locked"; }
        bind "Ctrl g" {
          Run "${pkgs.gex}/bin/gex" {
            floating true
            close_on_exit true
          }
        }
        bind "Ctrl a" {
          Run "${openProject}/bin/zellij-open-project" {
            cwd "/home/${username}"
            floating true
            close_on_exit true
          }
        }
      }
    }
  '';
  programs.zellij = {
    enable = true;
  };
}
