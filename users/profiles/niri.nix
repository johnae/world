{
  lib,
  config,
  pkgs,
  ...
}: let
  xcursor_theme = config.gtk.cursorTheme.name;

  swaylockTimeout = "300";
  swaylockSleepTimeout = "310";

  swaylockEffects = pkgs.writeShellApplication {
    name = "swaylock-effects";
    runtimeInputs = [pkgs.swaylock-effects];
    text = ''
      exec swaylock \
       --screenshots \
       --indicator-radius 100 \
       --indicator-thickness 7 \
       --effect-blur 15x3 \
       --effect-greyscale \
       --ring-color ffffff \
       --ring-clear-color baffba \
       --ring-ver-color bababa \
       --ring-wrong-color ffbaba \
       --key-hl-color bababa \
       --line-color ffffffaa \
       --inside-color ffffffaa \
       --inside-ver-color bababaaa \
       --line-ver-color bababaaa \
       --inside-clear-color baffbaaa \
       --line-clear-color baffbaaa \
       --inside-wrong-color ffbabaaa \
       --line-wrong-color ffbabaaa \
       --separator-color 00000000 \
       --grace 2 \
       --fade-in 0.2
    '';
  };

  swayidleCommand = pkgs.writeShellApplication {
    name = "swayidle";
    runtimeInputs = [pkgs.sway pkgs.bash swaylockEffects pkgs.swayidle];
    text = ''
      swayidle -d -w timeout ${swaylockTimeout} swaylock-effects \
                     timeout ${swaylockSleepTimeout} 'swaymsg "output * dpms off"' \
                     resume 'swaymsg "output * dpms on"' \
                     before-sleep swaylock-effects
    '';
  };
in {
  programs.niri.package = pkgs.niri-unstable;
  programs.niri.enable = true;
  home.packages = with pkgs; [
    fuzzel
    light
    pamixer
    scripts
    swaylockEffects
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

      # {
      #   matches = [
      #     {
      #       app-id = "^org\.wezfurlong\.wezterm$";
      #     }
      #   ];

      #   default-column-width = {};
      # }
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
      "Super+Alt+L".action.spawn = "swaylock-effects";
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

      # "Mod+Minus".action = set-column-width "-10%";
      # "Mod+Equal".action = set-column-width "+10%";

      # "Mod+Shift+Minus".action = set-window-height "-10%";
      # "Mod+Shift+Equal".action = set-window-height "+10%";

      # "Mod+V".action = toggle-window-floating;
      # "Mod+Shift+V".action = switch-focus-between-floating-and-tiling;

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

      "Ctrl+Alt+Delete".action = quit;

      "Mod+Shift+P".action = power-off-monitors;
    };
  };

  systemd.user.services = {
    swayidle-niri = {
      Unit = {
        Description = "When idle - do something :-)";
        After = "graphical-session.target";
        BindsTo = "graphical-session.target";
      };
      Service = {
        Type = "simple";
        ExecStart = "${swayidleCommand}/bin/swayidle";
      };
      Install.WantedBy = ["graphicalsession.target"];
    };
  };
}
