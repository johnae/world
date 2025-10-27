{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  xcursor_theme = config.gtk.cursorTheme.name;
  noctalia = inputs.noctalia.packages.${pkgs.system}.default;
  noctaliaIPC = "${noctalia}/bin/noctalia-shell ipc call";

  noctaliaInit = pkgs.writeShellApplication {
    name = "noctalia-init";
    text = ''
      PID="$(systemctl show --user noctalia --property=MainPID --value)"
      while ! [ -S "/var/run/user/1337/quickshell/by-pid/$PID/ipc.sock" ]; do
        sleep 0.1
      done
      ${noctaliaIPC} wallpaper random
      sleep 0.1
      ${noctaliaIPC} lockScreen toggle
    '';
  };

  openZellijSession = pkgs.writeShellApplication {
    name = "open-zellij-session";
    runtimeInputs = [pkgs.scripts pkgs.niri pkgs.jq];
    text = ''
      # Get window ID for a given title prefix
      # shellcheck disable=SC2088
      get_window_id() {
        local title_prefix="$1"
        niri msg -j windows | jq -r '.[] | select(.app_id == "wezterm_zellij_'"$title_prefix"'") | .id' | head -n1
      }
      DOMAIN="$(fuzzel-wezterm)"
      if [ -n "$DOMAIN" ]; then
        if [[ "$DOMAIN" == local* ]]; then
          PROJECT_PATH="$(list-projects ~/Development | fuzzel -d)"
        else
          PROJECT_PATH="$(list-projects "$DOMAIN:~/Development" | fuzzel -d)"
        fi
        PROJECT_NAME="$(basename "$PROJECT_PATH")"
        MAIN="''${PROJECT_NAME}_main"
        AUX="''${PROJECT_NAME}_aux"

        # Check if windows already exist and focus them if they do
        MAIN_WINDOW_ID=$(get_window_id "$MAIN")
        AUX_WINDOW_ID=$(get_window_id "$AUX")

        if [ -n "$MAIN_WINDOW_ID" ] && [ -n "$AUX_WINDOW_ID" ]; then
          niri msg action focus-window --id "$MAIN_WINDOW_ID"
          exit 0
        elif [ -n "$MAIN_WINDOW_ID" ]; then
          niri msg action focus-window --id "$MAIN_WINDOW_ID"
        elif [ -n "$AUX_WINDOW_ID" ]; then
          niri msg action focus-window --id "$AUX_WINDOW_ID"
        fi


        # Create zellij sessions in the appropriate context (remote or local)
        if [[ "$DOMAIN" == local* ]]; then
          # Create zellij sessions locally
          zellij attach -b "$AUX" options --default-cwd "$PROJECT_PATH" || true
          zellij attach -b "$MAIN" options --default-cwd "$PROJECT_PATH" || true

          # Open wezterm windows locally (only if they don't exist)
          if [ -z "''${MAIN_WINDOW_ID:-}" ]; then
            niri msg action spawn-sh -- "wezterm start --class 'wezterm_zellij_$MAIN' -- zellij attach '$MAIN'"
          fi
          if [ -z "''${AUX_WINDOW_ID:-}" ]; then
            niri msg action spawn-sh -- "wezterm start --class 'wezterm_zellij_$AUX' -- zellij attach '$AUX'"
          fi
        else
          # Create zellij sessions on remote host
          # shellcheck disable=SC2029
          echo "zellij attach -b '$AUX' options --default-cwd '$PROJECT_PATH' || true; zellij attach -b '$MAIN' options --default-cwd '$PROJECT_PATH' || true" | ssh "$DOMAIN" bash
          # Open wezterm windows via SSH (only if they don't exist)
          if [ -z "''${MAIN_WINDOW_ID:-}" ]; then
            niri msg action spawn-sh -- "wezterm ssh --class 'wezterm_zellij_$MAIN' '$DOMAIN' -- zellij attach '$MAIN'"
          fi
          if [ -z "''${AUX_WINDOW_ID:-}" ]; then
            niri msg action spawn-sh -- "wezterm ssh --class 'wezterm_zellij_$AUX' '$DOMAIN' -- zellij attach '$AUX'"
          fi
        fi
      fi
    '';
  };
in {
  services.swayidle = {
    enable = true;
    timeouts = [
      {
        timeout = 180;
        command = "${noctaliaIPC} lockScreen toggle";
      }
      {
        timeout = 180 * 3;
        command = "${pkgs.niri-unstable}/bin/niri msg action power-off-monitors";
      }
    ];
    events = [
      {
        event = "before-sleep";
        command = "${noctaliaIPC} lockScreen toggle";
      }
      {
        event = "after-resume";
        command = "${noctaliaIPC} lockScreen toggle";
      }
      {
        event = "after-resume";
        command = "${pkgs.niri-unstable}/bin/niri msg action power-on-monitors";
      }
    ];
  };

  programs.niri.package = pkgs.niri-unstable;
  programs.niri.enable = true;
  home.packages = with pkgs; [
    fuzzel
    light
    pamixer
    scripts
  ];

  programs.niri.settings = {
    input = {
      keyboard = {
        xkb = {
          layout = "us,se";
          model = "pc105";
          options = "ctrl:nocaps,grp:switch,compose:rctrl";
        };
        repeat-delay = 300;
        repeat-rate = 20;
      };
      touchpad = {
        tap = true;
        dwt = true;
        natural-scroll = true;
      };
      focus-follows-mouse.enable = true;
      focus-follows-mouse.max-scroll-amount = "0%";
      workspace-auto-back-and-forth = true;
    };

    spawn-at-startup = [
      # {
      #   sh = "${noctalia}/bin/noctalia-shell";
      # }
      {sh = "${noctaliaInit}/bin/noctalia-init";}
      {sh = "systemctl restart --user kanshi.service";}
    ];

    hotkey-overlay.skip-at-startup = true;

    cursor = {
      theme = xcursor_theme;
    };

    layout = {
      gaps = 16;
      center-focused-column = "never";
      preset-column-widths = [
        {proportion = 1.0 / 3.0;}
        {proportion = 1.0 / 2.0;}
        {proportion = 2.0 / 3.0;}
      ];
      default-column-width = {proportion = 0.5;};

      tab-indicator = {
        gap = 8;
        gaps-between-tabs = 4;
        corner-radius = 8;
        width = 10;
        position = "top";
      };

      focus-ring = {
        width = 4;

        active = {
          color = "#7fc8ff";
        };

        inactive = {
          color = "#505050";
        };
      };

      border = {
        enable = false;
      };
      shadow = {
        enable = true;
        softness = 30;
        spread = 5;
        offset = {
          x = 0;
          y = 5;
        };
        color = "#0007";
      };

      struts = {
        left = 8;
        right = 8;
        top = 8;
        bottom = 8;
      };
    };

    screenshot-path = "~/Sync/screenshots/%Y-%m-%dT%H.%M.%S.png";

    window-rules = [
      {
        geometry-corner-radius = {
          top-left = 12.0;
          top-right = 12.0;
          bottom-left = 12.0;
          bottom-right = 12.0;
        };
        clip-to-geometry = true;
      }

      {
        matches = [
          {
            app-id = "firefox$";
            title = "^Picture-in-Picture$";
          }
        ];
        open-floating = true;
      }
    ];

    binds = {
      "Mod+Shift+Slash".action.show-hotkey-overlay = [];
      "Super+Return".action.spawn = ["wezterm" "start" "--always-new-process"];
      # "Mod+D".action.spawn = "fuzzel";
      "Mod+D".action.spawn-sh = "${noctaliaIPC} launcher toggle";
      "Super+Alt+L".action.spawn-sh = "${noctaliaIPC} lockScreen toggle";
      "XF86AudioRaiseVolume" = {
        action.spawn = ["wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.1+"];
        allow-when-locked = true;
      };
      "XF86AudioLowerVolume" = {
        action.spawn = ["wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.1-"];
        allow-when-locked = true;
      };
      "XF86AudioMute" = {
        action.spawn = ["wpctl" "set-mute" "@DEFAULT_AUDIO_SINK@" "toggle"];
        allow-when-locked = true;
      };
      "XF86AudioMicMute" = {
        action.spawn = ["wpctl" "set-mute" "@DEFAULT_AUDIO_SOURCE@" "toggle"];
        allow-when-locked = true;
      };

      "Mod+Tab".action.toggle-overview = [];

      "Mod+Q".action.close-window = [];

      "Mod+Left".action.focus-column-left = [];
      "Mod+Down".action.focus-window-or-workspace-down = [];
      "Mod+Up".action.focus-window-or-workspace-up = [];
      "Mod+Right".action.focus-column-right = [];
      "Mod+H".action.focus-column-left = [];
      "Mod+J".action.focus-window-down = [];
      "Mod+K".action.focus-window-up = [];
      "Mod+L".action.focus-column-right = [];

      "Mod+Ctrl+Left".action.move-column-left = [];
      "Mod+Ctrl+Down".action.move-window-down = [];
      "Mod+Ctrl+Up".action.move-window-up = [];
      "Mod+Ctrl+Right".action.move-column-right = [];
      "Mod+Ctrl+H".action.move-column-left = [];
      "Mod+Ctrl+J".action.move-window-down = [];
      "Mod+Ctrl+K".action.move-window-up = [];
      "Mod+Ctrl+L".action.move-column-right = [];

      "Mod+Home".action.focus-column-first = [];
      "Mod+End".action.focus-column-last = [];
      "Mod+Ctrl+Home".action.move-column-to-first = [];
      "Mod+Ctrl+End".action.move-column-to-last = [];

      "Mod+Shift+Left".action.focus-monitor-left = [];
      "Mod+Shift+Down".action.focus-monitor-down = [];
      "Mod+Shift+Up".action.focus-monitor-up = [];
      "Mod+Shift+Right".action.focus-monitor-right = [];
      "Mod+Shift+H".action.focus-monitor-left = [];
      "Mod+Shift+J".action.focus-monitor-down = [];
      "Mod+Shift+K".action.focus-monitor-up = [];
      "Mod+Shift+L".action.focus-monitor-right = [];

      "Mod+Shift+Ctrl+Left".action.move-column-to-monitor-left = [];
      "Mod+Shift+Ctrl+Down".action.move-column-to-monitor-down = [];
      "Mod+Shift+Ctrl+Up".action.move-column-to-monitor-up = [];
      "Mod+Shift+Ctrl+Right".action.move-column-to-monitor-right = [];
      "Mod+Shift+Ctrl+H".action.move-column-to-monitor-left = [];
      "Mod+Shift+Ctrl+J".action.move-column-to-monitor-down = [];
      "Mod+Shift+Ctrl+K".action.move-column-to-monitor-up = [];
      "Mod+Shift+Ctrl+L".action.move-column-to-monitor-right = [];

      "Mod+Page_Down".action.focus-workspace-down = [];
      "Mod+Page_Up".action.focus-workspace-up = [];
      "Mod+U".action.focus-workspace-down = [];
      "Mod+I".action.focus-workspace-up = [];
      "Mod+Ctrl+Page_Down".action.move-column-to-workspace-down = [];
      "Mod+Ctrl+Page_Up".action.move-column-to-workspace-up = [];
      "Mod+Ctrl+U".action.move-column-to-workspace-down = [];
      "Mod+Ctrl+I".action.move-column-to-workspace-up = [];

      "Mod+Shift+Page_Down".action.move-workspace-down = [];
      "Mod+Shift+Page_Up".action.move-workspace-up = [];
      "Mod+Shift+U".action.move-workspace-down = [];
      "Mod+Shift+I".action.move-workspace-up = [];
      "Mod+Space".action.swap-window-left = [];
      "Mod+Shift+Space".action.swap-window-right = [];

      "Mod+WheelScrollDown" = {
        action.focus-workspace-down = [];
        cooldown-ms = 150;
      };
      "Mod+WheelScrollUp" = {
        action.focus-workspace-up = [];
        cooldown-ms = 150;
      };
      "Mod+Ctrl+WheelScrollDown" = {
        action.move-column-to-workspace-down = [];
        cooldown-ms = 150;
      };

      "Mod+Ctrl+WheelScrollUp" = {
        action.move-column-to-workspace-up = [];
        cooldown-ms = 150;
      };

      MouseForward.action.toggle-overview = [];

      "Mod+WheelScrollRight".action.focus-column-right = [];
      "Mod+WheelScrollLeft".action.focus-column-left = [];
      "Mod+Ctrl+WheelScrollRight".action.move-column-right = [];
      "Mod+Ctrl+WheelScrollLeft".action.move-column-left = [];

      "Mod+Shift+WheelScrollDown".action.focus-column-right = [];
      "Mod+Shift+WheelScrollUp".action.focus-column-left = [];
      "Mod+Ctrl+Shift+WheelScrollDown".action.move-column-right = [];
      "Mod+Ctrl+Shift+WheelScrollUp".action.move-column-left = [];

      "Mod+1".action.focus-workspace = 1;
      "Mod+2".action.focus-workspace = 2;
      "Mod+3".action.focus-workspace = 3;
      "Mod+4".action.focus-workspace = 4;
      "Mod+5".action.focus-workspace = 5;
      "Mod+6".action.focus-workspace = 6;
      "Mod+7".action.focus-workspace = 7;
      "Mod+8".action.focus-workspace = 8;
      "Mod+9".action.focus-workspace = 9;

      "Mod+Ctrl+1".action.move-column-to-workspace = 1;
      "Mod+Ctrl+2".action.move-column-to-workspace = 2;
      "Mod+Ctrl+3".action.move-column-to-workspace = 3;
      "Mod+Ctrl+4".action.move-column-to-workspace = 4;
      "Mod+Ctrl+5".action.move-column-to-workspace = 5;
      "Mod+Ctrl+6".action.move-column-to-workspace = 6;
      "Mod+Ctrl+7".action.move-column-to-workspace = 7;
      "Mod+Ctrl+8".action.move-column-to-workspace = 8;
      "Mod+Ctrl+9".action.move-column-to-workspace = 9;

      "Mod+BracketLeft".action.consume-or-expel-window-left = [];
      "Mod+BracketRight".action.consume-or-expel-window-right = [];

      "Mod+Comma".action.consume-window-into-column = [];
      "Mod+Period".action.expel-window-from-column = [];

      "Mod+R".action.switch-preset-column-width = [];
      "Mod+Shift+R".action.switch-preset-window-height = [];
      "Mod+Ctrl+R".action.reset-window-height = [];
      "Mod+F".action.maximize-column = [];
      "Mod+Shift+F".action.fullscreen-window = [];

      "Mod+Ctrl+F".action.expand-column-to-available-width = [];

      "Mod+C".action.center-column = [];

      "Mod+W".action.toggle-column-tabbed-display = [];

      "Mod+Shift+w".action.spawn = ["sh" "-c" "${openZellijSession}/bin/open-zellij-session"];
      "Mod+Minus".action.spawn = ["sh" "-c" "${pkgs.scripts}/bin/rofi-rbw"];
      "Mod+Shift+Minus".action.spawn = ["sh" "-c" "passonly=y ${pkgs.scripts}/bin/rofi-rbw"];
      "Mod+Shift+Equal".action.spawn = ["sh" "-c" "codeonly=y ${pkgs.scripts}/bin/rofi-rbw"];

      "Print".action.screenshot = [];
      "Ctrl+Print".action.screenshot-screen = [];
      "Alt+Print".action.screenshot-window = [];

      "Mod+Escape" = {
        allow-inhibiting = false;
        action.toggle-keyboard-shortcuts-inhibit = [];
      };

      # "Mod+p".action.spawn = ["sh" "-c" "${systemMenu}/bin/niri-system-menu"];
      "Mod+p".action.spawn-sh = "${noctaliaIPC} sessionMenu toggle";

      "Ctrl+Alt+Delete".action.quit = [];

      "Mod+Shift+P".action.power-off-monitors = [];
    };
  };
}
