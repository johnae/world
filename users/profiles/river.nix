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

  inherit (config) gtk;

  input = listToAttrs (map (name: {
      inherit name;
      value = {
        disable-while-typing = "enabled";
        natural-scroll = "enabled";
        tap = "enabled";
      };
    }) [
      "1739:52804:MSFT0001:00_06CB:CE44_Touchpad"
      "1739:30383:DLL075B:01_06CB:76AF_Touchpad"
      "1739:30383:DELL07E6:00_06CB:76AF_Touchpad"
      "1739:52710:DLL096D:01_06CB:CDE6_Touchpad"
      "1739:52620:SYNA8005:00_06CB:CD8C_Touchpad"
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
  xcursor_theme = gtk.cursorTheme.name;
in {
  home.packages = [
    pkgs.kile
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
    NIXOS_OZONE_WL = "1";
  };
  wayland.windowManager.river.enable = true;
  wayland.windowManager.river.xkb = {
    default_layout = "us,se";
    default_options = "ctrl:nocaps,grp:switch";
    default_model = "pc105";
    default_variant = "";
  };

  wayland.windowManager.river.settings = {
    inherit input;

    map.normal.Super.Return = "spawn alacritty";
    map.normal.Super.D = "spawn 'rofi -show combi -modes combi -combi-modes \"drun,run\"'";
    map.normal.Super.Q = "close";

    map.normal."Super+Shift".E = "spawn 'emacsclient -c -n -a='";

    map.normal.Super.J = "focus-view next";
    map.normal.Super.K = "focus-view previous";

    map.normal.Super.Left = "focus-view next";
    map.normal.Super.Right = "focus-view previous";

    map.normal."Super+Shift".J = "swap next";
    map.normal."Super+Shift".K = "swap previous";

    map.normal."Super+Control".J = ["focus-view next" "zoom"];
    map.normal."Super+Control".K = ["focus-view previous" "zoom"];

    map.normal.Super.Minus = "spawn 'rofi-rbw'";
    map.normal."Super+Shift".Minus = "spawn 'passonly=y rofi-rbw'";

    map.normal.Super.T = "spawn 'rofi-spotify-search track'";
    map.normal.Super.P = "spawn 'rofi-spotify-search playlist'";
    map.normal."Super+Shift".N = "spawn 'spotify-cmd next'";
    map.normal."Super+Shift".P = "spawn 'spotify-cmd prev'";
    map.normal."Super+Shift".M = "spawn 'spotify-cmd pause'";

    map.normal."Super+Shift".R = "spawn 'systemctl --user restart graphical-session.target'";

    map.normal.Super.Period = "focus-output next";
    map.normal.Super.Comma = "focus-output previous";

    map.normal."Super+Shift".Period = "send-to-output next";
    map.normal."Super+Shift".Comma = "send-to-output previous";

    map.normal.Super.Space = "zoom";

    map.normal."Super+Alt".H = "move left 100";
    map.normal."Super+Alt".J = "move down 100";
    map.normal."Super+Alt".K = "move up 100";
    map.normal."Super+Alt".L = "move right 100";

    map.normal."Super+Alt+Control".H = "snap left";
    map.normal."Super+Alt+Control".J = "snap down";
    map.normal."Super+Alt+Control".K = "snap up";
    map.normal."Super+Alt+Control".L = "snap right";
    map.normal."Super+Alt+Shift".H = "resize horizontal -100";
    map.normal."Super+Alt+Shift".J = "resize vertical 100";
    map.normal."Super+Alt+Shift".K = "resize vertical -100";
    map.normal."Super+Alt+Shift".L = "resize horizontal 100";

    map-pointer.normal.Super.BTN_LEFT = "move-view";
    map-pointer.normal.Super.BTN_RIGHT = "resize-view";

    map.normal."Super+Control".Space = "toggle-float";
    map.normal.Super.F = "toggle-fullscreen";

    ### kile layouts ###

    map.normal."Super+Control".Up = ''send-layout-cmd kile "focused U ((h: v v) 1 0.65 0)"'';
    map.normal."Super+Control".Down = ''send-layout-cmd kile "focused D ((h: v v) 1 0.65 1)"'';
    map.normal."Super+Control".Left = ''send-layout-cmd kile "focused L ((v: h h) 1 0.65 0)"'';
    map.normal."Super+Control".Right = ''send-layout-cmd kile "focused R ((v: h h) 1 0.65 1)"'';
    map.normal."Super+Control".D = ''send-layout-cmd kile "focused Deck deck"'';
    map.normal."Super+Control".F = ''send-layout-cmd kile "focused Full full"'';

    ####################

    additional-modes = ["passthrough" "wmctl"];

    map.normal.Super.F11 = "enter-mode passthrough";
    map.passthrough.Super.F11 = "enter-mode normal";

    map.normal.Super.Escape = "spawn '${river-menu}/bin/river-menu'";

    #map.normal.Super.Escape = "enter-mode wmctl";
    #map.wmctl.Super.Escape = "enter-mode normal";
    #map.wmctl.Super.P = ["enter-mode normal" "spawn \"systemctl poweroff\""];
    #map.wmctl.Super.H = ["enter-mode normal" "spawn \"systemctl hibernate\""];
    #map.wmctl.Super.R = ["enter-mode normal" "spawn \"systemctl reboot\""];
    #map.wmctl.Super.S = ["enter-mode normal" "spawn \"systemctl suspend-then-hibernate\""];
    #map.wmctl.Super.L = ["enter-mode normal" "exit"];

    background-color = "0x002b36";
    border-color-focused = "0x93a1a1";
    border-color-unfocused = "0x586e75";

    set-repeat = "50 300";

    float-filters.app-id = ["float"];
    float-filters.title = ["popup title with spaces"];
    csd-filters.app-id = ["gedit"];

    focus-follows-cursor = "normal";

    default-layout = "kile";
    exec = [
      "systemctl restart --user xdg-desktop-portal-gtk || true"
      "systemctl restart --user kanshi.service || true"
      "${pkgs.swaybg}/bin/swaybg -o '*' -i /home/john/Pictures/wallpaper.jpg -m fill &"
      "${pkgs.eww-wayland}/bin/eww kill && ${pkgs.eww-wayland}/bin/eww open bar"
    ];
    layout-generator-exec = "kile --namespace kile --layout '((v: h h) 1 0.65 1)'";
  };
}
