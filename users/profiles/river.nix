{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit
    (lib // builtins)
    listToAttrs
    ;

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
    runtimeInputs = [pkgs.bash swaylockEffects pkgs.swayidle];
    text = ''
      exec swayidle -d -w timeout ${swaylockTimeout} swaylock-effects \
                     timeout ${swaylockSleepTimeout} '${pkgs.wlopm}/bin/wlopm --off *' \
                     resume '${pkgs.wlopm}/bin/wlopm --on *' \
                     before-sleep swaylock-effects
    '';
  };

  screenshot = pkgs.writeShellApplication {
    name = "screenshot";
    runtimeInputs = [pkgs.slurp pkgs.grim];
    text = ''
      mkdir -p ~/Sync/screenshots
      slurp | grim -g - ~/Sync/screenshots/"$(date +'%Y-%m-%dT%H.%M.%S.png')"
    '';
  };

  xcursor_theme = config.gtk.cursorTheme.name;
  terminal-bin = "${pkgs.alacritty}/bin/alacritty";

  _dev-env = {
    name,
    host ? null,
  }:
    pkgs.writeShellApplication {
      inherit name;
      runtimeInputs = with pkgs; [alacritty];
      text = ''
        # shellcheck disable=SC2093,SC2016
        exec alacritty --class=${name} \
                       --working-directory="$HOME" \
        ${
          if host == null
          then ''
            --command zellij -s ${name} attach -c -f ${name}
          ''
          else ''
            --command ssh -A -t ${host} 'ln -sf $env.SSH_AUTH_SOCK $"/run/user/(id -u)/ssh-auth.sock"; zellij -s ${name} attach -c -f ${name}'
          ''
        }
      '';
    };

  dev-env = {
    name,
    host ? null,
  }:
    pkgs.writeShellApplication {
      inherit name;
      runtimeInputs = with pkgs; [alacritty jq lswt];
      text = ''
        if ! lswt -j | jq -e '.[] | select(.app_id == "${name}")' > /dev/null; then
          # shellcheck disable=SC2093,SC2016
          riverctl spawn '${_dev-env {inherit name host;}}/bin/${name}'
        fi
        exec riverctl set-focused-tags 1
      '';
    };

  local-dev = dev-env {name = "local-dev";};
  remote-dev = dev-env {
    name = "remote-dev";
    host = config.userinfo.devRemote;
  };

  input = listToAttrs (map (name: {
      inherit name;
      value = {
        disable-while-typing = "enabled";
        natural-scroll = "enabled";
        tap = "enabled";
      };
    }) [
      "pointer-1267-12850-ELAN06A1:00_04F3:3232_Touchpad"
      "pointer-1739-30383-DELL07E6:00_06CB:76AF_Touchpad"
      "pointer-1739-30383-DLL075B:01_06CB:76AF_Touchpad"
      "pointer-1739-52620-SYNA8005:00_06CB:CD8C_Touchpad"
      "pointer-1739-52710-DLL096D:01_06CB:CDE6_Touchpad"
      "pointer-1739-52804-MSFT0001:00_06CB:CE44_Touchpad"
    ]);

  river-menu = pkgs.writeShellApplication {
    name = "river-menu";
    runtimeInputs = [pkgs.rofi-wayland];
    text = ''

      ACTION="$(echo -e "logout\npoweroff\nreboot\nhibernate\nsuspend" | rofi -normal-window -matching fuzzy -i -dmenu)"
      if [ "$ACTION" = "logout" ]; then
        riverctl exit
      elif [ "$ACTION" = "poweroff" ]; then
        systemctl poweroff
      elif [ "$ACTION" = "hibernate" ]; then
        systemctl hibernate
      elif [ "$ACTION" = "reboot" ]; then
        systemctl reboot
      elif [ "$ACTION" = "suspend" ]; then
        systemctl suspend-then-hibernate
      else
        echo "Unknown action" 1>&2
      fi
    '';
  };
in {
  home.packages = [
    pkgs.kile-wl
    pkgs.rofi-wayland
    pkgs.scripts
  ];
  home.sessionVariables = {
    GDK_BACKEND = "wayland";
    CLUTTER_BACKEND = "wayland";
    QT_QPA_PLATFORM = "wayland-egl";
    QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
    MOZ_ENABLE_WAYLAND = "1";
    MOZ_USE_XINPUT2 = "1";
    QT_WAYLAND_FORCE_DPI = "physical";
    SDL_VIDEODRIVER = "wayland";
    XCURSOR_THEME = xcursor_theme;
    QT_STYLE_OVERRIDE = lib.mkForce "gtk";
    _JAVA_AWT_WM_NONREPARENTING = "1";
  };

  wayland.windowManager.river.enable = true;

  wayland.windowManager.river.settings = {
    inherit input;

    keyboard-layout = "-model pc105 -variant '' -options ctrl:nocaps,grp:switch,compose:rctrl us,se";

    map.normal.Super.Return = "spawn ${terminal-bin}";
    map.normal.Super.D = "spawn '${pkgs.rofi-wayland}/bin/rofi -show combi -modes combi -combi-modes \"drun,run\"'";

    map.normal."Super+Shift".Q = "close";

    map.normal."Super+Shift".E = "spawn '${local-dev}/bin/local-dev'";
    map.normal."Super+Shift".R = "spawn '${remote-dev}/bin/remote-dev'";
    map.normal."Super+Shift".S = "spawn '${screenshot}/bin/screenshot'";

    map.normal."Super".K = "spawn '${pkgs.kanshi}/bin/kanshictl reload'";

    map.normal."Super+Control".L = "spawn '${swaylockEffects}/bin/swaylock-effects'";

    map.normal.Super.Left = "focus-view left";
    map.normal.Super.Right = "focus-view right";
    map.normal.Super.Up = "focus-view up";
    map.normal.Super.Down = "focus-view down";

    map.normal."Super+Shift".J = "swap next";
    map.normal."Super+Shift".K = "swap previous";

    map.normal."Super+Control".J = ["focus-view next" "swap next" "zoom"];
    map.normal."Super+Control".K = ["focus-view previous" "zoom"];
    map.normal."Super+Shift".Space = "send-layout-cmd luatile 'next_layout()'";

    map.normal.Super.Minus = "spawn '${pkgs.scripts}/bin/rofi-rbw'";
    map.normal."Super+Shift".Minus = "spawn 'passonly=y ${pkgs.scripts}/bin/rofi-rbw'";

    map.normal.Super.M = "focus-output next";

    map.normal."Super+Shift".M = "send-to-output next";

    map.normal.Super.Space = "zoom";

    map.normal."Super+Control".Space = "toggle-float";
    map.normal.Super.F = "toggle-fullscreen";

    additional-modes = ["passthrough" "wmctl"];

    map.normal.Super.F11 = "enter-mode passthrough";
    map.passthrough.Super.F11 = "enter-mode normal";

    map.normal.Super.Escape = "spawn '${river-menu}/bin/river-menu'";

    background-color = "0x002b36";
    border-color-focused = "0x93a1a1";
    border-color-unfocused = "0x586e75";

    set-repeat = "50 300";
    focus-follows-cursor = "always";
    set-cursor-warp = "on-focus-change";

    rule-add = [
      "-app-id 'remote-dev' tags '1'"
      "-app-id 'local-dev' tags '1'"
      "-app-id 'chromium-browser (/home/john/.config/chromium-work)' tags '2'"
      "-app-id 'firefox' tags '4'"
      "-app-id 'chromium-browser (/home/john/.config/chromium-private)' tags '8'"
      "-app-id 'teams-for-linux' tags '256'"
      "-app-id 'chromium-browser *' ssd"
      "-app-id 'org.gnome.Nautilus' ssd"
      "-app-id 'firefox' ssd"
    ];

    default-layout = "luatile";
    layout-generator-exec = "${pkgs.river-luatile}/bin/river-luatile";
    exec = [
      "${pkgs.wpaperd}/bin/wpaperd"
      "${swayidleCommand}/bin/swayidle"
      "${pkgs.kanshi}/bin/kanshi"
    ];
  };
}
