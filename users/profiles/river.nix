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
  terminal-bin = "${pkgs.wezterm}/bin/wezterm start --always-new-process";

  _dev-env = {
    name,
    domain ? null,
  }:
    pkgs.writeShellApplication {
      inherit name;
      runtimeInputs = with pkgs; [wezterm];
      text =
        if domain == null
        then ''
          exec wezterm start --class ${name} --always-new-process
        ''
        else ''
          exec wezterm start --class ${name} --domain ${name} --always-new-process --attach
        '';
    };

  dev-env = {
    name,
    tag,
    domain ? null,
  }:
    pkgs.writeShellApplication {
      inherit name;
      runtimeInputs = with pkgs; [jq lswt];
      text = ''
        if ! lswt -j | jq -e '.toplevels[] | select(."app-id" == "${name}")' > /dev/null; then
          # shellcheck disable=SC2093,SC2016
          riverctl spawn '${_dev-env {inherit name domain;}}/bin/${name}'
        fi
        exec riverctl set-focused-tags ${toString tag}
      '';
    };

  local-dev = dev-env {
    name = "local-dev";
    tag = 128;
  };

  remote-dev = dev-env {
    name = "remote-dev";
    tag = 256;
    domain = "remote-dev";
  };

  river-menu = pkgs.writeShellApplication {
    name = "river-menu";
    runtimeInputs = [pkgs.fuzzel];
    text = ''

      ACTION="$(echo -e "logout\npoweroff\nreboot\nhibernate\nsuspend" | fuzzel -d)"
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
  home.packages = with pkgs; [
    kile-wl
    rofi-wayland
    fuzzel
    light
    pamixer
    scripts
  ];
  home.sessionVariables = {
    GDK_BACKEND = "wayland";
    CLUTTER_BACKEND = "wayland";
    QT_QPA_PLATFORM = "";
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
    input = listToAttrs (map (name: {
        inherit name;
        value = {
          disable-while-typing = true;
          natural-scroll = true;
          tap = true;
        };
      }) [
        "pointer-1267-12850-ELAN06A1:00_04F3:3232_Touchpad"
        "pointer-1739-30383-DELL07E6:00_06CB:76AF_Touchpad"
        "pointer-1739-30383-DLL075B:01_06CB:76AF_Touchpad"
        "pointer-1739-52620-SYNA8005:00_06CB:CD8C_Touchpad"
        "pointer-1739-52710-DLL096D:01_06CB:CDE6_Touchpad"
        "pointer-1739-52804-MSFT0001:00_06CB:CE44_Touchpad"
        "pointer-2362-628-PIXA3854:00_093A:0274_Touchpad"
      ]);

    declare-mode = ["passthrough" "wmctl" "locked" "normal"];

    keyboard-layout = "-model pc105 -variant '' -options ctrl:nocaps,grp:switch,compose:rctrl us,se";

    map.normal."Super+Control L" = "spawn '${swaylockEffects}/bin/swaylock-effects'";
    map.normal."Super+Control Space" = "toggle-float";

    map.normal."Super+Control J" = "spawn 'riverctl focus-view next; riverctl swap next; riverctl zoom'";
    map.normal."Super+Control K" = "spawn 'riverctl focus-view previous; riverctl zoom'";
    map.normal."Super Up" = "focus-view up";
    map.normal."Super Down" = "focus-view down";
    map.normal."Super Left" = "focus-view left";
    map.normal."Super Right" = "focus-view right";
    map.normal."Super V" = "focus-view next";
    map.normal."Super+Shift V" = "focus-view previous";
    map.normal."Super M" = "focus-output next";

    map.normal."Super+Shift J" = "swap next";
    map.normal."Super+Shift K" = "swap previous";

    map.normal."Super+Shift M" = "send-to-output next";

    map.normal."Super+Shift Q" = "close";
    map.normal."Super Space" = "zoom";

    map.normal."Super D" = "spawn '${pkgs.fuzzel}/bin/fuzzel'";
    map.normal."Super Escape" = "spawn '${river-menu}/bin/river-menu'";
    map.normal."Super K" = "spawn '${pkgs.kanshi}/bin/kanshictl reload'";

    map.normal."Super Minus" = "spawn '${pkgs.scripts}/bin/fuzzel-rbw'";
    map.normal."Super Return" = "spawn '${terminal-bin}'";
    map.normal."Super+Shift E" = "spawn '${local-dev}/bin/local-dev'";

    map.normal."Super+Shift Minus" = "spawn 'passonly=y ${pkgs.scripts}/bin/fuzzel-rbw'";
    map.normal."Super+Shift R" = "spawn '${remote-dev}/bin/remote-dev'";

    map.normal."Super+Shift Space" = "send-layout-cmd luatile 'next_tag_layout()'";

    map.normal."Super+Shift Up" = "send-layout-cmd luatile 'adjust_main_ratio_by(0.05)'";
    map.normal."Super+Shift Down" = "send-layout-cmd luatile 'adjust_main_ratio_by(-0.05)'";
    map.normal."Super+Shift S" = "spawn '${screenshot}/bin/screenshot'";

    map.normal."Super F11" = "enter-mode passthrough";
    map.normal."Super F" = "toggle-fullscreen";
    map.passthrough."Super F11" = "enter-mode normal";

    map.normal."None XF86Eject" = "spawn 'eject -T'";
    map.normal."None XF86AudioRaiseVolume" = "spawn 'pamixer -i 5'";
    map.normal."None XF86AudioLowerVolume" = "spawn 'pamixer -d 5'";
    map.normal."None XF86AudioMute" = "spawn 'pamixer --toggle-mute'";
    map.normal."None XF86AudioMedia" = "spawn 'playerctl play-pause'";
    map.normal."None XF86AudioPlay" = "spawn 'playerctl play-pause'";
    map.normal."None XF86AudioPrev" = "spawn 'playerctl previous'";
    map.normal."None XF86AudioNext" = "spawn 'playerctl next'";
    map.normal."None XF86MonBrightnessUp" = "spawn 'light -A 5'";
    map.normal."None XF86MonBrightnessDown" = "spawn 'light -U 5'";

    map.locked."None XF86Eject" = "spawn 'eject -T'";
    map.locked."None XF86AudioRaiseVolume" = "spawn 'pamixer -i 5'";
    map.locked."None XF86AudioLowerVolume" = "spawn 'pamixer -d 5'";
    map.locked."None XF86AudioMute" = "spawn 'pamixer --toggle-mute'";
    map.locked."None XF86AudioMedia" = "spawn 'playerctl play-pause'";
    map.locked."None XF86AudioPlay" = "spawn 'playerctl play-pause'";
    map.locked."None XF86AudioPrev" = "spawn 'playerctl previous'";
    map.locked."None XF86AudioNext" = "spawn 'playerctl next'";
    map.locked."None XF86MonBrightnessUp" = "spawn 'light -A 5'";
    map.locked."None XF86MonBrightnessDown" = "spawn 'light -U 5'";

    background-color = "0x002b36";
    border-color-focused = "0x93a1a1";
    border-color-unfocused = "0x586e75";

    set-repeat = "50 300";
    focus-follows-cursor = "always";
    set-cursor-warp = "on-focus-change";

    rule-add = [
      "-app-id 'chromium-browser (/home/john/.config/chromium-work)' tags '2'"
      "-app-id 'firefox' tags '4'"
      "-app-id 'chromium-browser (/home/john/.config/chromium-private)' tags '8'"
      "-app-id 'local-dev' tags '128'"
      "-app-id 'remote-dev' tags '256'"
      "-app-id 'chromium-browser *' ssd"
      "-app-id 'org.gnome.Nautilus' ssd"
      "-app-id 'firefox' ssd"
    ];

    default-layout = "luatile";
    spawn =
      [
        "${swayidleCommand}/bin/swayidle"
        "${pkgs.kanshi}/bin/kanshi"
        "${pkgs.wpaperd}/bin/wpaperd"
      ]
      ++
      ## layout gen
      [
        "${pkgs.river-luatile}/bin/river-luatile"
      ];
  };

  wayland.windowManager.river.extraConfig = ''
    for i in $(seq 1 9)
    do
        tags=$((1 << (i - 1)))
        riverctl map normal Super "$i" set-focused-tags $tags
        riverctl map normal Super+Shift "$i" set-view-tags $tags
        riverctl map normal Super+Control "$i" toggle-focused-tags $tags
        riverctl map normal Super+Shift+Control "$i" toggle-view-tags $tags
    done

    all_tags=$(((1 << 32) - 1))
    riverctl map normal Super 0 set-focused-tags "$all_tags"
    riverctl map normal Super+Shift 0 set-view-tags "$all_tags"
  '';
}
