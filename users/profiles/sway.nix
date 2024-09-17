{
  pkgs,
  config,
  lib,
  ...
}: let
  swayservice = Description: ExecStart: {
    Unit = {
      inherit Description;
      After = "sway-session.target";
      BindsTo = "sway-session.target";
    };

    Service = {
      Type = "simple";
      inherit ExecStart;
    };

    Install.WantedBy = ["sway-session.target"];
  };

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

  screenshot = pkgs.writeShellApplication {
    name = "screenshot";
    runtimeInputs = [pkgs.slurp pkgs.grim];
    text = ''
      mkdir -p ~/Pictures/screenshots
      slurp | grim -g - ~/Pictures/screenshots/"$(date +'%Y-%m-%dT%H.%M.%S.png')"
    '';
  };

  swayOnReload = pkgs.writeShellApplication {
    name = "sway-on-reload";
    runtimeInputs = [pkgs.sway];
    text = ''
      LID=/proc/acpi/button/lid/LID
      if [ ! -e "$LID" ]; then
        LID=/proc/acpi/button/lid/LID0
      fi
      if [ -e "$LID" ]; then
        if grep -q open "$LID"/state; then
            swaymsg output eDP-1 enable
        else
            swaymsg output eDP-1 disable
        fi
      fi

      ${
        lib.optionalString config.services.kanshi.enable
        ''
          systemctl restart --user kanshi.service
        ''
      }

    '';
  };

  terminal-bin = "${pkgs.wezterm}/bin/wezterm";
  editor = ''${terminal-bin} start --class=hx hx'';

  fonts = {
    names = ["Roboto" "Font Awesome 5 Free" "Font Awesome 5 Brands" "Arial" "sans-serif"];
    style = "Bold";
    size = 10.0;
  };

  modifier = "Mod4";

  xcursor_theme = config.gtk.cursorTheme.name;
in {
  home.packages = with pkgs; [
    kile-wl
    rofi-wayland
    fuzzel
    light
    pamixer
    scripts
    persway
  ];
  home.sessionVariables = {
    GDK_BACKEND = "wayland";
    CLUTTER_BACKEND = "wayland";
    QT_QPA_PLATFORM = "";
    QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
    QT_WAYLAND_FORCE_DPI = "physical";
    SDL_VIDEODRIVER = "wayland";
    MOZ_ENABLE_WAYLAND = "1";
    MOZ_USE_XINPUT2 = "1";
    XCURSOR_THEME = xcursor_theme;
    QT_STYLE_OVERRIDE = lib.mkForce "gtk";
    _JAVA_AWT_WM_NONREPARENTING = "1";
  };

  wayland.windowManager.sway = {
    enable = true;
    systemd.enable = true;
    config = {
      inherit fonts modifier;
      # output = {
      #   "*" = {
      #     bg = "~/Pictures/wallpaper.jpg fill";
      #   };
      # };

      seat = {
        "*" = {
          hide_cursor = "when-typing enable";
          inherit xcursor_theme;
        };
      };

      focus = {
        followMouse = true;
        newWindow = "smart";
        mouseWarping = true;
      };

      workspaceAutoBackAndForth = true;

      window = let
        command = "floating enable, resize set width 100ppt height 120ppt";
        floatCommand = "floating enable";
      in {
        titlebar = false;
        border = 3;
        hideEdgeBorders = "smart";
        commands = [
          {
            inherit command;
            criteria.class = "scripts";
          }
          {
            inherit command;
            criteria.title = "scripts";
          }
          {
            inherit command;
            criteria.app_id = "scripts";
          }
          {
            command = floatCommand;
            criteria.class = "input-window";
          }
          {
            command = floatCommand;
            criteria.class = "gcr-prompter";
          }
          {
            command = "inhibit_idle fullscreen";
            criteria.shell = ".*";
          }
          # {
          #   command = "kill";
          #   criteria.title = "Firefox - Sharing Indicator";
          # }
        ];
      };

      floating = {
        titlebar = false;
        border = 3;
      };

      input = {
        "type:keyboard" = {
          xkb_layout = "us,se";
          xkb_model = "pc105";
          xkb_options = "ctrl:nocaps,grp:switch,compose:rctrl";
          xkb_variant = "\"\"";
        };
        "1739:52804:MSFT0001:00_06CB:CE44_Touchpad" = {
          dwt = "true";
          natural_scroll = "true";
          tap = "true";
        };
        "1739:30383:DLL075B:01_06CB:76AF_Touchpad" = {
          dwt = "true";
          natural_scroll = "true";
          tap = "true";
        };
        "1739:30383:DELL07E6:00_06CB:76AF_Touchpad" = {
          dwt = "true";
          natural_scroll = "true";
          tap = "true";
        };
        "1739:52710:DLL096D:01_06CB:CDE6_Touchpad" = {
          dwt = "true";
          natural_scroll = "true";
          tap = "true";
        };
        "1267:12850:ELAN06A1:00_04F3:3232_Touchpad" = {
          dwt = "true";
          natural_scroll = "true";
          tap = "true";
        };
        "1739:52620:SYNA8005:00_06CB:CD8C_Touchpad" = {
          dwt = "true";
          natural_scroll = "true";
          tap = "true";
        };
      };

      gaps = {
        inner = 4;
        top = 4;
        bottom = 4;
        left = 4;
        right = 4;
      };

      modes = {
        resize = {
          Left = "resize shrink width 10 px or 10 ppt";
          Right = "resize grow width 10 px or 10 ppt";
          Up = "resize shrink height 10 px or 10 ppt";
          Down = "resize grow height 10 px or 10 ppt";
          Return = "mode default";
          Escape = "mode default";
        };

        "disabled keybindings" = {
          "${modifier}+Shift+Control+x" = "mode default";
        };

        "(p)oweroff, (s)uspend, (h)ibernate, (r)eboot, (l)ogout" = {
          p = "exec swaymsg 'mode default' && systemctl poweroff";
          s = "exec swaymsg 'mode default' && systemctl suspend-then-hibernate";
          h = "exec swaymsg 'mode default' && systemctl hibernate";
          r = "exec swaymsg 'mode default' && systemctl reboot";
          l = "exec swaymsg 'mode default' && systemctl --user stop sway-session.target && systemctl --user stop graphical-session.target && swaymsg exit";
          Return = "mode default";
          Escape = "mode default";
        };
      };

      keybindings = lib.mkOptionDefault {
        "${modifier}+Escape" = ''mode "(p)oweroff, (s)uspend, (h)ibernate, (r)eboot, (l)ogout"'';
        "${modifier}+Shift+x" = ''mode "disabled keybindings"'';
        "${modifier}+r" = ''mode "resize"'';

        # "${modifier}+Control+Tab" = "[con_mark=_swap] unmark _swap; mark --add _swap; [con_mark=_prev] focus; swap container with mark _swap; [con_mark=_swap] unmark _swap";
        # "${modifier}+Control+Left" = "[con_mark=_swap] unmark _swap; mark --add _swap; focus left; swap container with mark _swap; [con_mark=_swap] unmark _swap";
        # "${modifier}+Control+Right" = "[con_mark=_swap] unmark _swap; mark --add _swap; focus right; swap container with mark _swap; [con_mark=_swap] unmark _swap";
        # "${modifier}+Control+Down" = "[con_mark=_swap] unmark _swap; mark --add _swap; focus down; swap container with mark _swap; [con_mark=_swap] unmark _swap";

        "${modifier}+space" = "exec persway stack-swap-main";
        "${modifier}+Control+space" = "exec persway stack-main-rotate-next";

        "${modifier}+Tab" = "exec persway stack-focus-next";
        "${modifier}+Shift+Tab" = "exec persway stack-focus-prev";

        "${modifier}+z" = "exec persway change-layout spiral";
        "${modifier}+x" = "exec persway change-layout stack-main --size 70";
        "${modifier}+c" = "exec persway change-layout stack-main --size 70 --stack-layout tiled";
        "${modifier}+v" = "exec persway change-layout manual";

        "${modifier}+Shift+s" = ''exec ${screenshot}/bin/screenshot'';

        "${modifier}+Shift+k" = ''exec systemctl --user restart kanshi'';

        "${modifier}+Control+l" = ''exec ${swaylockEffects}/bin/swaylock-effects'';

        "${modifier}+Control+Shift+l" = ''exec ${pkgs.psmisc}/bin/killall -USR1 swayidle'';

        "${modifier}+i" = ''exec ${pkgs.sway}/bin/swaymsg inhibit_idle open'';
        "${modifier}+Shift+i" = ''exec ${pkgs.sway}/bin/swaymsg inhibit_idle none'';

        "${modifier}+Return" = ''exec ${terminal-bin}'';
        "${modifier}+d" = ''exec ${pkgs.rofi-wayland}/bin/rofi -show combi -modes combi -combi-modes "drun,run"'';

        "${modifier}+minus" = ''exec ${pkgs.scripts}/bin/rofi-rbw'';
        "${modifier}+Shift+minus" = ''exec passonly=y ${pkgs.scripts}/bin/rofi-rbw'';

        "${modifier}+Shift+e" = ''exec ${editor}'';

        "${modifier}+Shift+v" = ''splith'';

        "${modifier}+m" = ''move workspace to output right'';
        "${modifier}+Shift+q" = ''kill'';

        XF86MonBrightnessUp = ''exec light -A 5'';
        XF86MonBrightnessDown = ''exec light -U 5'';

        "${modifier}+q" = ''layout stacking'';
        "${modifier}+o" = ''move absolute position center'';
        "${modifier}+a" = ''focus parent'';
      };

      startup = [
        {
          command = "${pkgs.xorg.xrdb}/bin/xrdb -merge ~/.Xresources";
        }
        {
          command = "${pkgs.gnome-settings-daemon}/libexec/gsd-xsettings";
        }
        {
          command = "${pkgs.dbus.out}/bin/dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY SWAYSOCK XDG_CURRENT_DESKTOP";
        }
        {
          command = "${pkgs.polkit_gnome.out}/libexec/polkit-gnome-authentication-agent-1";
        }
        {
          command = "${swayOnReload}/bin/sway-on-reload";
          always = true;
        }
      ];

      bars = [];
    };
    extraConfig = ''
      no_focus [window_role="browser"]
      popup_during_fullscreen smart
      bindswitch --reload --locked lid:on output eDP-1 disable
      bindswitch --reload --locked lid:off output eDP-1 enable
      titlebar_border_thickness 0
    '';
  };

  systemd.user.services = {
    persway = swayservice "Small Sway IPC Deamon" "${pkgs.persway}/bin/persway daemon -w -e '[tiling] opacity 1' -f '[tiling] opacity 0.95; opacity 1' -l 'mark --add _prev' -d stack_main";
    # rotating-background = swayservice "Rotating background service for Sway" "${rotatingBackground}/bin/rotating-background art,abstract,space";
    wpaperd = swayservice "Sway BG service" "${pkgs.wpaperd}/bin/wpaperd";
    swayidle = swayservice "Sway Idle Service - lock screen etc" "${swayidleCommand}/bin/swayidle";
  };
}
