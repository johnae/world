{
  inputs,
  config,
  lib,
  pkgs,
  ...
}: let
  hyprService = Description: ExecStart: {
    Unit = {
      inherit Description;
      After = "hypr-session.target";
      BindsTo = "hypr-session.target";
    };

    Service = {
      Type = "simple";
      inherit ExecStart;
    };

    Install.WantedBy = ["hypr-session.target"];
  };

  swaylockTimeout = "300";
  swaylockSleepTimeout = "310";

  swaylockEffects = pkgs.writeShellApplication {
    name = "swaylock-effects";
    runtimeInputs = [pkgs.swaylock-effects];
    text = ''
      swaylock \
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
      swayidle -d -w timeout ${swaylockTimeout} swaylock-effects \
                     timeout ${swaylockSleepTimeout} 'hyprctl dispatch dpms off' \
                     resume 'hyprctl dispatch dpms on' \
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
  terminal = pkgs.alacritty;
  terminal-bin = "${pkgs.alacritty}/bin/alacritty";
  terminal-emacs = ''${terminal-bin} --class=emacs -e emacsclient -t -a=""'';
in {
  xdg.configFile."wpaperd/wallpaper.toml".source = pkgs.writeText "wallpaper.toml" ''
    [default]
    path = "~/Sync/wallpapers"
    duration = "30m"
    sorting = "random"
    apply-shadow = false
  '';

  home.sessionVariables = {
    GDK_BACKEND = "wayland";
    CLUTTER_BACKEND = "wayland";
    QT_QPA_PLATFORM = "wayland-egl";
    QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
    QT_WAYLAND_FORCE_DPI = "physical";
    SDL_VIDEODRIVER = "wayland";
    MOZ_ENABLE_WAYLAND = "1";
    MOZ_USE_XINPUT2 = "1";
    XCURSOR_THEME = xcursor_theme;
    QT_STYLE_OVERRIDE = lib.mkForce "gtk";
    _JAVA_AWT_WM_NONREPARENTING = "1";
    NIXOS_OZONE_WL = "1";
  };

  wayland.windowManager.hyprland.enable = true;
  wayland.windowManager.hyprland.package = pkgs.hyprland-unstable;
  wayland.windowManager.hyprland.extraConfig = ''
    bind=$mod,escape,submap,(p)oweroff, (s)uspend, (h)ibernate, (r)eboot, (l)ogout
    submap=(p)oweroff, (s)uspend, (h)ibernate, (r)eboot, (l)ogout

    bind=,p,exec,systemctl poweroff
    bind=,p,submap,reset

    bind=,s,exec,systemctl suspend-then-hibernate
    bind=,s,submap,reset

    bind=,h,exec,systemctl hibernate
    bind=,h,submap,reset

    bind=,r,exec,systemctl reboot
    bind=,r,submap,reset

    bind=,l,exit
    bind=,l,submap,reset

    bind=,escape,submap,reset
    bind=,return,submap,reset
    submap=reset
  '';
  wayland.windowManager.hyprland.settings = {
    "$mod" = "SUPER";
    bind =
      [
        "$mod, Return, exec, ${terminal-bin}"
        "$mod SHIFT, q, killactive"
        "$mod, d, exec, ${pkgs.rofi-wayland}/bin/rofi -show combi -modes combi -combi-modes \"drun,run\""
        "$mod SHIFT, e, exec, ${terminal-emacs}"
        "$mod SHIFT, s, exec, ${screenshot}/bin/screenshot"
        "$mod CONTROL, l, exec, ${swaylockEffects}/bin/swaylock-effects"
        "$mod, left, movefocus, l"
        "$mod, right, movefocus, r"
        "$mod, up, movefocus, u"
        "$mod, down, movefocus, d"
        "$mod, m, movecurrentworkspacetomonitor, +1"
        "$mod, minus, exec, ${pkgs.scripts}/bin/rofi-rbw"
        "$mod SHIFT, minus, exec, passonly=y ${pkgs.scripts}/bin/rofi-rbw"
      ]
      ++ (map (num: "$mod, ${num}, workspace, ${num}") (builtins.genList (x: builtins.toString (x + 1)) 9))
      ++ (map (num: "$mod SHIFT, ${num}, movetoworkspace, ${num}") (builtins.genList (x: builtins.toString (x + 1)) 9))
      ++ [
        "$mod, 0, workspace, 10"
        "$mod SHIFT, 0, movetoworkspace, 10"
        "$mod, g, togglegroup"
        "$mod, Tab, changegroupactive"
        "$mod SHIFT, left, movewindoworgroup, l"
        "$mod SHIFT, right, movewindoworgroup, r"
        "$mod SHIFT, up, movewindoworgroup, u"
        "$mod SHIFT, down, movewindoworgroup, d"
        "$mod, space, layoutmsg, swapwithmaster"
        "$mod, f, fullscreen"
        "$mod SHIFT, f, fakefullscreen"
      ];

    misc.disable_hyprland_logo = true;
    misc.disable_splash_rendering = true;
    misc.groupbar_titles_font_size = 12;
    misc.groupbar_gradients = true; ## ugly

    binds = {
      workspace_back_and_forth = true;
      allow_workspace_cycles = true;
    };

    animations = {
      enabled = true;
      animation = [
        "workspaces,1,0.6,default"
        "windows,1,0.8,default"
        "fade,1,0.8,default"
        "border,1,0.6,default"
        "borderangle,1,0.6,default"
      ];
    };

    gestures = {
      workspace_swipe = true;
    };

    general = {
      layout = "master";
      border_size = 0;
      gaps_in = 8;
      gaps_out = 16;
      "col.active_border" = "0xf0f000aa";
      "col.inactive_border" = "0x00000000";
      "col.group_border" = "0x2E344000";
      "col.group_border_active" = "0x5E81AC00";
    };

    decoration = {
      rounding = 8;
      blur = {
        enabled = true;
        size = 7;
        passes = 2;
        xray = true;
        ignore_opacity = true;
        new_optimizations = true;
        noise = 0.12;
        contrast = 1.05;
        brightness = 0.8;
      };
      drop_shadow = true;
      shadow_range = 20;
      shadow_render_power = 2;
      "col.shadow" = "0x99000000";
      "col.shadow_inactive" = "0x55000000";
      active_opacity = 0.95;
      inactive_opacity = 0.89;
      fullscreen_opacity = 1.0;
    };

    dwindle = {
      # See https://wiki.hyprland.org/Configuring/Dwindle-Layout/ for more
      pseudotile = true; # master switch for pseudotiling. Enabling is bound to mainMod + P in the keybinds section below
      preserve_split = true; # you probably want this
    };

    master = {
      # See https://wiki.hyprland.org/Configuring/Master-Layout/ for more
      new_is_master = true;
      mfact = 0.7;
      orientation = "right";
    };

    layerrule = "blur,waybar";

    input = {
      kb_layout = "us,se";
      kb_variant = "";
      kb_model = "pc105";
      kb_options = "ctrl:nocaps,grp:switch";
      kb_rules = "";

      follow_mouse = 1;

      touchpad = {
        natural_scroll = true;
        disable_while_typing = true;
        tap-to-click = true;
      };
    };
    exec = [
      "${pkgs.kanshi}/bin/kanshi"
    ];
    exec-once = [
      "${pkgs.wpaperd}/bin/wpaperd"
    ];
  };
}
