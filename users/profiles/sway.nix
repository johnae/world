{ pkgs, config, lib, ... }:
let

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

    Install.WantedBy = [ "sway-session.target" ];
  };

  swaylockTimeout = "300";
  swaylockSleepTimeout = "310";

  swayidleCommand = lib.concatStringsSep " " [
    "${pkgs.swayidle}/bin/swayidle -w"
    "timeout ${swaylockTimeout}"
    "'${pkgs.swaylock-dope}/bin/swaylock-dope'"
    "timeout ${swaylockSleepTimeout}"
    "'${pkgs.sway}/bin/swaymsg \"output * dpms off\"'"
    "resume '${pkgs.sway}/bin/swaymsg \"output * dpms on\"'"
    "before-sleep '${pkgs.swaylock-dope}/bin/swaylock-dope'"
  ];

  toggleKeyboardLayouts = pkgs.writeStrictShellScriptBin "toggle-keyboard-layouts" ''
    export PATH=${pkgs.jq}/bin''${PATH:+:}$PATH
    current_layout="$(swaymsg -t get_inputs -r | jq -r "[.[] | select(.xkb_active_layout_name != null)][0].xkb_active_layout_name")"
    if [ "$current_layout" = "English (US)" ]; then
    swaymsg 'input "*" xkb_layout se,us'
    else
    swaymsg 'input "*" xkb_layout us,se'
    fi
  '';

  randomPicsumBackground = pkgs.writeStrictShellScriptBin "random-picsum-background" ''
    category=''${1:-nature}
    ${pkgs.curl}/bin/curl --silent --fail-with-body -Lo /tmp/wallpaper.jpg 'https://source.unsplash.com/featured/3200x1800/?'"$category" 2>/dev/null
    if [ -e "$HOME"/Pictures/wallpaper.jpg ]; then
    mv "$HOME"/Pictures/wallpaper.jpg "$HOME"/Pictures/previous-wallpaper.jpg
    fi
    mv /tmp/wallpaper.jpg "$HOME"/Pictures/wallpaper.jpg
    echo "$HOME"/Pictures/wallpaper.jpg
  '';

  swayBackground = pkgs.writeStrictShellScriptBin "sway-background" ''
    category=''${1:-nature,abstract,space}
    BG=$(${randomPicsumBackground}/bin/random-picsum-background "$category")
    exec swaymsg "output * bg '$BG' fill"
  '';

  rotatingBackground = pkgs.writeStrictShellScriptBin "rotating-background" ''
    category=''${1:-art,abstract,space}
    while true; do
    if ! ${swayBackground}/bin/sway-background "$category"; then
      if [ -e "$HOME/Pictures/wallpaper.jpg" ] && [ "$(stat -c "$HOME/Pictures/wallpaper.jpg")" -ge 50000 ]; then
        exec swaymsg "output * bg '$HOME/Pictures/wallpaper.jpg' fill"
      else
        exec swaymsg "output * bg '$HOME/Pictures/default-background.jpg' fill"
      fi
    fi
    sleep 600
    done
  '';

  swayFocusWindow = pkgs.writeStrictShellScriptBin "sway-focus-window" ''
    export SK_OPTS="--no-bold --color=bw  --height=40 --reverse --no-hscroll --no-mouse"
    window="$(${pkgs.sway}/bin/swaymsg -t get_tree | \
              ${pkgs.jq}/bin/jq -r '.nodes | .[] | .nodes | . [] | select(.nodes != null) | .nodes | .[] | select(.name != null) | "\(.id?) \(.name?)"' | \
              ${pkgs.scripts}/bin/sk-sk | \
              awk '{print $1}')"
    ${pkgs.sway}/bin/swaymsg "[con_id=$window] focus"
  '';

  swayOnReload = pkgs.writeStrictShellScriptBin "sway-on-reload" ''
    LID=/proc/acpi/button/lid/LID
    if [ ! -e "$LID" ]; then
      LID=/proc/acpi/button/lid/LID0
    fi
    if [ ! -e "$LID" ]; then
       echo No lid found - skipping sway reload action
       exit
    fi
    if grep -q open "$LID"/state; then
        swaymsg output eDP-1 enable
    else
        swaymsg output eDP-1 disable
    fi

    ${lib.optionalString config.services.kanshi.enable
    ''
    systemctl restart --user kanshi.service
    ''
    }

  '';

  fonts = {
    names = [ "Roboto" "Font Awesome 5 Free" "Font Awesome 5 Brands" "Arial" "sans-serif" ];
    style = "Bold";
    size = 10.0;
  };

  modifier = "Mod4";

in
{

  home.sessionVariables = {
    GDK_BACKEND = "wayland";
    CLUTTER_BACKEND = "wayland";
    QT_QPA_PLATFORM = "wayland-egl";
    MOZ_ENABLE_WAYLAND = "1";
    MOZ_USE_XINPUT2 = "1";
    XCURSOR_THEME = "default";
    QT_STYLE_OVERRIDE = "gtk";
    _JAVA_AWT_WM_NONREPARENTING = "1";
  };

  wayland.windowManager.sway = {
    enable = true;
    systemdIntegration = true;
    config = {
      inherit fonts modifier;
      output = {
        "*" = {
          bg = "~/Pictures/wallpaper.jpg fill";
        };
      };

      focus = {
        followMouse = true;
        newWindow = "smart";
        mouseWarping = true;
      };

      workspaceAutoBackAndForth = true;

      window =
        let
          command = "floating enable, resize set width 100ppt height 120ppt";
          floatCommand = "floating enable";
        in
        {
          titlebar = false;
          border = 3;
          hideEdgeBorders = "smart";
          commands = [
            { inherit command; criteria.class = "scripts"; }
            { inherit command; criteria.title = "scripts"; }
            { inherit command; criteria.app_id = "scripts"; }
            { command = floatCommand; criteria.class = "input-window"; }
            { command = floatCommand; criteria.class = "gcr-prompter"; }
            { command = "inhibit_idle fullscreen"; criteria.shell = ".*"; }
            { command = "kill"; criteria.title = "Firefox - Sharing Indicator"; }
          ];
        };

      floating = {
        titlebar = false;
        border = 3;
      };

      input = {
        "*" = {
          xkb_layout = "us,se";
          xkb_model = "pc105";
          xkb_options = "ctrl:nocaps,grp:switch";
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
        "1739:52620:SYNA8005:00_06CB:CD8C_Touchpad" = {
          dwt = "true";
          natural_scroll = "true";
          tap = "true";
        };
      };

      gaps = {
        inner = 4;
        top = -5;
        bottom = -5;
        left = -5;
        right = -5;
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
          Escape = "mode default";
        };

        "(p)oweroff, (s)uspend, (h)ibernate, (r)eboot, (l)ogout" = {
          p = "exec swaymsg 'mode default' && systemctl poweroff";
          s = "exec swaymsg 'mode default' && systemctl suspend-then-hibernate";
          h = "exec swaymsg 'mode default' && systemctl hibernate";
          r = "exec swaymsg 'mode default' && systemctl reboot";
          l = "exec swaymsg 'mode default' && swaymsg exit";
          Return = "mode default";
          Escape = "mode default";
        };

      };

      keybindings = lib.mkOptionDefault {
        "${modifier}+Escape" = ''mode "(p)oweroff, (s)uspend, (h)ibernate, (r)eboot, (l)ogout"'';
        "${modifier}+x" = ''mode "disabled keybindings"'';
        "${modifier}+r" = ''mode "resize"'';

        "${modifier}+Tab" = ''exec ${pkgs.scripts}/bin/scripts ${swayFocusWindow}/bin/sway-focus-window'';

        "${modifier}+t" = ''exec ${pkgs.scripts}/bin/rofi-spotify-search track'';
        "${modifier}+p" = ''exec ${pkgs.scripts}/bin/rofi-spotify-search playlist'';
        "${modifier}+Shift+n" = ''exec ${pkgs.scripts}/bin/spotify-cmd next'';
        "${modifier}+Shift+p" = ''exec ${pkgs.scripts}/bin/spotify-cmd prev'';
        "${modifier}+Shift+m" = ''exec ${pkgs.scripts}/bin/spotify-cmd pause'';

        "${modifier}+Shift+k" = ''exec ${pkgs.systemd}/bin/systemctl --user restart kanshi'';
        "${modifier}+Control+k" = ''exec ${toggleKeyboardLayouts}/bin/toggle-keyboard-layouts'';

        "${modifier}+Control+l" = ''exec ${pkgs.swaylock-dope}/bin/swaylock-dope'';

        "${modifier}+Control+Shift+l" = ''exec ${pkgs.psmisc}/bin/killall -USR1 swayidle'';

        "${modifier}+i" = ''exec ${pkgs.sway}/bin/swaymsg inhibit_idle open'';
        "${modifier}+Shift+i" = ''exec ${pkgs.sway}/bin/swaymsg inhibit_idle none'';

        "${modifier}+Shift+Return" = ''exec ${pkgs.my-emacs}/bin/emacsclient -c -n -a= --eval '(jae/eshell-new)' '';
        "${modifier}+Return" = '' exec ${pkgs.alacritty}/bin/alacritty'';
        "${modifier}+d" = ''exec ${pkgs.rofi-wayland}/bin/rofi -show drun'';

        "${modifier}+minus" = ''exec ${pkgs.scripts}/bin/rofi-rbw'';
        "${modifier}+Shift+minus" = ''exec passonly=y ${pkgs.scripts}/bin/rofi-rbw'';

        "${modifier}+b" = ''exec ${swayBackground}/bin/sway-background'';

        "${modifier}+Shift+e" = ''exec ${pkgs.my-emacs}/bin/emacsclient -c -n -a='';

        "${modifier}+Shift+v" = ''splith'';

        "${modifier}+m" = ''move workspace to output right'';
        "${modifier}+Shift+q" = ''kill'';

        XF86MonBrightnessUp = ''exec light -A 5'';
        XF86MonBrightnessDown = ''exec light -U 5'';

        XF86AudioNext = ''exec ${pkgs.scripts}/bin/spotify-cmd next'';
        XF86AudioPrev = ''exec ${pkgs.scripts}/bin/spotify-cmd prev'';
        XF86AudioPlay = ''exec ${pkgs.scripts}/bin/spotify-cmd play'';
        XF86AudioPause = ''exec ${pkgs.scripts}/bin/spotify-cmd pause'';

        "${modifier}+q" = ''layout stacking'';
        "${modifier}+o" = ''move absolute position center'';
        "${modifier}+a" = ''focus parent'';
      };

      startup = [
        {
          command = "${pkgs.xorg.xrdb}/bin/xrdb -merge ~/.Xresources";
        }
        {
          command = "${pkgs.gnome3.gnome-settings-daemon}/libexec/gsd-xsettings";
        }
        {
          command = "${pkgs.dbus_tools}/bin/dbus-update-activation-environment 2>/dev/null && ${pkgs.dbus_tools}/bin/dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY SWAYSOCK";
        }
        {
          command = "${swayOnReload}/bin/sway-on-reload";
          always = true;
        }
      ];

      bars = [
        {
          inherit fonts;
          extraConfig = ''
            height 25
          '';
          statusCommand = "${pkgs.i3status-rust}/bin/i3status-rs ~/.config/i3status-rust/config-default.toml";
          colors = {
            background = "#2E3440AA";
            statusline = "#88C0D0";
            separator = "#3B4252";

            focusedWorkspace = {
              border = "#88C0D0";
              background = "#88C0D0";
              text = "#2E3440";
            };

            activeWorkspace = {
              border = "#4C566ADD";
              background = "#4C566ADD";
              text = "#D8DEE9";
            };

            inactiveWorkspace = {
              border = "#3B4252DD";
              background = "#3B4252DD";
              text = "#E5E9F0";
            };

            urgentWorkspace = {
              border = "#B48EAD";
              background = "#B48EAD";
              text = "#ECEFF4";
            };

            bindingMode = {
              border = "#BF616A";
              background = "#BF616A";
              text = "#E5E9F0";
            };
          };
        }
      ];
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
    persway = swayservice "Small Sway IPC Deamon" "${pkgs.persway}/bin/persway -w -a -e '[tiling] opacity 1' -f '[tiling] opacity 0.95; opacity 1'";
    rotating-background = swayservice "Rotating background service for Sway" "${rotatingBackground}/bin/rotating-background art,abstract,space";
    swayidle = swayservice "Sway Idle Service - lock screen etc" swayidleCommand;
  };

}
