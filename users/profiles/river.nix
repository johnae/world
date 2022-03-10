{ config, lib, ... }:

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
    wayland.windowManager.river.enable = true;
    wayland.windowManager.river.xkb = {
      default_layout = "us,se";
      default_options = "ctrl:nocaps,grp:switch";
      default_model = "pc105";
      default_variant = "";
    };
    wayland.windowManager.river.settings = {
        map.normal."Super+Shift" = "Return spawn alacritty";
        map.normal."Super D" = "spawn 'rofi -show drun'";
        map.normal."Super Q" = "close";
        map.normal."Super+Shift E " = "exit";

        map.normal."Super J" = "focus-view next";
        map.normal."Super K" = "focus-view previous";

        map.normal."Super+Shift J" = "swap next";
        map.normal."Super+Shift K" = "swap previous";

        map.normal."Super Period" = "focus-output next";
        map.normal."Super Comma" = "focus-output previous";

        map.normal."Super+Shift Period" = "send-to-output next";
        map.normal."Super+Shift Comma" = "send-to-output previous";

        map.normal."Super Return" = "zoom";

        map.normal."Super H" = "send-layout-cmd rivertile \"main-ratio -0.05\"";
        map.normal."Super L" = "send-layout-cmd rivertile \"main-ratio +0.05\"";

        map.normal."Super+Shift H" = "send-layout-cmd rivertile \"main-count +1\"";
        map.normal."Super+Shift L" = "send-layout-cmd rivertile \"main-count -1\"";

        map.normal."Super+Alt H" = "move left 100";
        map.normal."Super+Alt J" = "move down 100";
        map.normal."Super+Alt K" = "move up 100";
        map.normal."Super+Alt L" = "move right 100";

        map.normal."Super+Alt+Control H" = "snap left";
        map.normal."Super+Alt+Control J" = "snap down";
        map.normal."Super+Alt+Control K" = "snap up";
        map.normal."Super+Alt+Control L" = "snap right";
        map.normal."Super+Alt+Shift H" = "resize horizontal -100";
        map.normal."Super+Alt+Shift J" = "resize vertical 100";
        map.normal."Super+Alt+Shift K" = "resize vertical -100";
        map.normal."Super+Alt+Shift L" = "resize horizontal 100";

        map-pointer.normal."Super BTN_LEFT" = "move-view";
        map-pointer.normal."Super BTN_RIGHT" = "resize-view";

        map.normal."Super Space" = "toggle-float";
        map.normal."Super F" = "toggle-fullscreen";
        map.normal."Super Up" = "send-layout-cmd rivertile \"main-location top\"";
        map.normal."Super Right" = "send-layout-cmd rivertile \"main-location right\"";
        map.normal."Super Down" = "send-layout-cmd rivertile \"main-location down\"";
        map.normal."Super Left" = "send-layout-cmd rivertile \"main-location left\"";

        additional-modes = [ "passthrough" ];
        map.normal."Super F11" = "enter-mode passthrough";
        map.passthrough."Super F11" = "enter-mode normal";

        background-color = "0x002b36";
        border-color-focused = "0x93a1a1";
        border-color-unfocused = "0x586e75";

        repeat = "50 300";

        float-filters.app-id = [ "float" ];
        float-filters.title = [ "popup title with spaces" ];
        csd-filters.app-id = [ "gedit" ];

        focus-follows-cursor = true;

        default-layout = "rivertile";
        layout-generator-exec = "rivertile -view-padding 4 -outer-padding 4";

    };
}
