{
  lib,
  config,
  pkgs,
  ...
}: let
  xcursor_theme = config.gtk.cursorTheme.name;
  swayidleConf = config.services.swayidle;

  systemMenu = pkgs.writeShellApplication {
    name = "niri-system-menu";
    runtimeInputs = [pkgs.niri pkgs.fuzzel];
    text = ''
      ACTION="$(cat<<EOF | fuzzel -d
      poweroff
      reboot
      suspend
      exit
      EOF
      )"
      if [ -z "$ACTION" ]; then
        exit
      fi
      if [ "$ACTION" = "exit" ]; then
        niri msg action "$ACTION"
        exit
      fi
      systemctl "$ACTION"
    '';
  };

  openWeztermDomain = pkgs.writeShellApplication {
    name = "open-wezterm-domain";
    runtimeInputs = [pkgs.scripts pkgs.niri pkgs.jq];
    text = ''
      DOMAIN="$(fuzzel-wezterm)"
      if [ -n "$DOMAIN" ]; then
        WINDOW_ID="$(niri msg -j windows | jq -r '.[] | select(.app_id == "'"wez-remote-$DOMAIN"'").id')"
        if [ -n "$WINDOW_ID" ]; then
          niri msg action focus-window --id "$WINDOW_ID"
        else
          niri msg action spawn -- wezterm start --domain "$DOMAIN" --attach --always-new-process --class "wez-remote-$DOMAIN"
        fi
      fi
    '';
  };
in {
  services.swayidle = {
    timeouts = [
      {
        timeout = (builtins.head swayidleConf.timeouts) + 90;
        command = "${pkgs.niri-unstable}/bin/niri msg action power-off-monitors";
      }
    ];
    events = [
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
  programs.niri.settings = with config.lib.niri.actions; {
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

    environment = {
      GDK_BACKEND = "wayland";
      CLUTTER_BACKEND = "wayland";
      QT_QPA_PLATFORM = "";
      QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
      QT_WAYLAND_FORCE_DPI = "physical";
      SDL_VIDEODRIVER = "wayland";
      MOZ_ENABLE_WAYLAND = "1";
      MOZ_USE_XINPUT2 = "1";
      NIXOS_OZONE_WL = "1";
      XCURSOR_THEME = xcursor_theme;
      QT_STYLE_OVERRIDE = lib.mkForce "gtk";
      _JAVA_AWT_WM_NONREPARENTING = "1";
    };

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
      "Mod+Shift+Slash".action = show-hotkey-overlay;
      "Super+Return".action.spawn = ["wezterm" "start" "--always-new-process"];
      "Mod+D".action.spawn = "fuzzel";
      "Super+Alt+L".action.spawn = "loginctl lock-sessions";
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

      "Mod+Tab".action = toggle-overview;

      "Mod+Q".action = close-window;

      "Mod+Left".action = focus-column-left;
      "Mod+Down".action = focus-window-down;
      "Mod+Up".action = focus-window-up;
      "Mod+Right".action = focus-column-right;
      "Mod+H".action = focus-column-left;
      "Mod+J".action = focus-window-down;
      "Mod+K".action = focus-window-up;
      "Mod+L".action = focus-column-right;

      "Mod+Ctrl+Left".action = move-column-left;
      "Mod+Ctrl+Down".action = move-window-down;
      "Mod+Ctrl+Up".action = move-window-up;
      "Mod+Ctrl+Right".action = move-column-right;
      "Mod+Ctrl+H".action = move-column-left;
      "Mod+Ctrl+J".action = move-window-down;
      "Mod+Ctrl+K".action = move-window-up;
      "Mod+Ctrl+L".action = move-column-right;

      "Mod+Home".action = focus-column-first;
      "Mod+End".action = focus-column-last;
      "Mod+Ctrl+Home".action = move-column-to-first;
      "Mod+Ctrl+End".action = move-column-to-last;

      "Mod+Shift+Left".action = focus-monitor-left;
      "Mod+Shift+Down".action = focus-monitor-down;
      "Mod+Shift+Up".action = focus-monitor-up;
      "Mod+Shift+Right".action = focus-monitor-right;
      "Mod+Shift+H".action = focus-monitor-left;
      "Mod+Shift+J".action = focus-monitor-down;
      "Mod+Shift+K".action = focus-monitor-up;
      "Mod+Shift+L".action = focus-monitor-right;

      "Mod+Shift+Ctrl+Left".action = move-column-to-monitor-left;
      "Mod+Shift+Ctrl+Down".action = move-column-to-monitor-down;
      "Mod+Shift+Ctrl+Up".action = move-column-to-monitor-up;
      "Mod+Shift+Ctrl+Right".action = move-column-to-monitor-right;
      "Mod+Shift+Ctrl+H".action = move-column-to-monitor-left;
      "Mod+Shift+Ctrl+J".action = move-column-to-monitor-down;
      "Mod+Shift+Ctrl+K".action = move-column-to-monitor-up;
      "Mod+Shift+Ctrl+L".action = move-column-to-monitor-right;

      "Mod+Page_Down".action = focus-workspace-down;
      "Mod+Page_Up".action = focus-workspace-up;
      "Mod+U".action = focus-workspace-down;
      "Mod+I".action = focus-workspace-up;
      "Mod+Ctrl+Page_Down".action = move-column-to-workspace-down;
      "Mod+Ctrl+Page_Up".action = move-column-to-workspace-up;
      "Mod+Ctrl+U".action = move-column-to-workspace-down;
      "Mod+Ctrl+I".action = move-column-to-workspace-up;

      "Mod+Shift+Page_Down".action = move-workspace-down;
      "Mod+Shift+Page_Up".action = move-workspace-up;
      "Mod+Shift+U".action = move-workspace-down;
      "Mod+Shift+I".action = move-workspace-up;
      "Mod+Space".action = swap-window-left;
      "Mod+Shift+Space".action = swap-window-right;

      "Mod+WheelScrollDown" = {
        action = focus-workspace-down;
        cooldown-ms = 150;
      };
      "Mod+WheelScrollUp" = {
        action =
          focus-workspace-up;
        cooldown-ms = 150;
      };
      "Mod+Ctrl+WheelScrollDown" = {
        action = move-column-to-workspace-down;
        cooldown-ms = 150;
      };

      "Mod+Ctrl+WheelScrollUp" = {
        action =
          move-column-to-workspace-up;
        cooldown-ms = 150;
      };

      MouseForward.action = toggle-overview;

      "Mod+WheelScrollRight".action = focus-column-right;
      "Mod+WheelScrollLeft".action = focus-column-left;
      "Mod+Ctrl+WheelScrollRight".action = move-column-right;
      "Mod+Ctrl+WheelScrollLeft".action = move-column-left;

      "Mod+Shift+WheelScrollDown".action = focus-column-right;
      "Mod+Shift+WheelScrollUp".action = focus-column-left;
      "Mod+Ctrl+Shift+WheelScrollDown".action = move-column-right;
      "Mod+Ctrl+Shift+WheelScrollUp".action = move-column-left;

      "Mod+1".action = focus-workspace 1;
      "Mod+2".action = focus-workspace 2;
      "Mod+3".action = focus-workspace 3;
      "Mod+4".action = focus-workspace 4;
      "Mod+5".action = focus-workspace 5;
      "Mod+6".action = focus-workspace 6;
      "Mod+7".action = focus-workspace 7;
      "Mod+8".action = focus-workspace 8;
      "Mod+9".action = focus-workspace 9;

      # "Mod+Ctrl+1".action = move-column-to-workspace 1;
      # "Mod+Ctrl+2".action = move-column-to-workspace 2;
      # "Mod+Ctrl+3".action = move-column-to-workspace 3;
      # "Mod+Ctrl+4".action = move-column-to-workspace 4;
      # "Mod+Ctrl+5".action = move-column-to-workspace 5;
      # "Mod+Ctrl+6".action = move-column-to-workspace 6;
      # "Mod+Ctrl+7".action = move-column-to-workspace 7;
      # "Mod+Ctrl+8".action = move-column-to-workspace 8;
      # "Mod+Ctrl+9".action = move-column-to-workspace 9;
      ## see: https://github.com/sodiboo/niri-flake/issues/1018

      "Mod+Ctrl+1".action.spawn = ["niri" "msg" "action" "move-column-to-workspace" (toString 1)];
      "Mod+Ctrl+2".action.spawn = ["niri" "msg" "action" "move-column-to-workspace" (toString 2)];
      "Mod+Ctrl+3".action.spawn = ["niri" "msg" "action" "move-column-to-workspace" (toString 3)];
      "Mod+Ctrl+4".action.spawn = ["niri" "msg" "action" "move-column-to-workspace" (toString 4)];
      "Mod+Ctrl+5".action.spawn = ["niri" "msg" "action" "move-column-to-workspace" (toString 5)];
      "Mod+Ctrl+6".action.spawn = ["niri" "msg" "action" "move-column-to-workspace" (toString 6)];
      "Mod+Ctrl+7".action.spawn = ["niri" "msg" "action" "move-column-to-workspace" (toString 7)];
      "Mod+Ctrl+8".action.spawn = ["niri" "msg" "action" "move-column-to-workspace" (toString 8)];
      "Mod+Ctrl+9".action.spawn = ["niri" "msg" "action" "move-column-to-workspace" (toString 9)];

      "Mod+BracketLeft".action = consume-or-expel-window-left;
      "Mod+BracketRight".action = consume-or-expel-window-right;

      "Mod+Comma".action = consume-window-into-column;
      "Mod+Period".action = expel-window-from-column;

      "Mod+R".action = switch-preset-column-width;
      "Mod+Shift+R".action = switch-preset-window-height;
      "Mod+Ctrl+R".action = reset-window-height;
      "Mod+F".action = maximize-column;
      "Mod+Shift+F".action = fullscreen-window;

      "Mod+Ctrl+F".action = expand-column-to-available-width;

      "Mod+C".action = center-column;

      "Mod+Shift+w".action.spawn = ["sh" "-c" "${openWeztermDomain}/bin/open-wezterm-domain"];
      "Mod+Minus".action.spawn = ["sh" "-c" "${pkgs.scripts}/bin/rofi-rbw"];
      "Mod+Shift+Minus".action.spawn = ["sh" "-c" "passonly=y ${pkgs.scripts}/bin/rofi-rbw"];
      "Mod+Shift+Equal".action.spawn = ["sh" "-c" "codeonly=y ${pkgs.scripts}/bin/rofi-rbw"];

      "Mod+W".action = toggle-column-tabbed-display;

      "Print".action = screenshot;
      # "Ctrl+Print".action = screenshot-screen;
      "Alt+Print".action = screenshot-window;

      "Mod+Escape" = {
        allow-inhibiting = false;
        action = toggle-keyboard-shortcuts-inhibit;
      };

      "Mod+p".action.spawn = ["sh" "-c" "${systemMenu}/bin/niri-system-menu"];

      "Ctrl+Alt+Delete".action = quit;

      "Mod+Shift+P".action = power-off-monitors;
    };
  };
}
