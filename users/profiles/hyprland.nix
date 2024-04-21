{
  config, ## this is null?
  lib,
  pkgs,
  ...
}: let
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

  swapCycle = dir:
    pkgs.writeShellApplication {
      name = "swap-${dir}";
      runtimeInputs = [pkgs.hyprland-unstable pkgs.jq];
      text = ''
        WS="$(hyprctl activeworkspace -j | jq -r .id)"
        STACKLEN="$(hyprctl clients -j | jq '[.[] | select(.workspace.id == '"$WS"' and .hidden == false)] | length - 2')"
        if [ "$STACKLEN" -le 0 ]; then
          STACKLEN=0
        fi
        hyprctl dispatch layoutmsg focusmaster master
        hyprctl dispatch layoutmsg cycle${dir}
        hyprctl dispatch layoutmsg swapwithmaster master
        hyprctl dispatch layoutmsg focusmaster master
        hyprctl dispatch layoutmsg cycle${dir}
        # shellcheck disable=SC2034
        for i in $(seq 1 "$STACKLEN"); do
          hyprctl dispatch layoutmsg swap${dir}
        done
        hyprctl dispatch layoutmsg focusmaster master
      '';
    };

  swapCycleNext = swapCycle "next";
  swapCyclePrev = swapCycle "prev";

  xcursor_theme = config.gtk.cursorTheme.name;
  terminal-bin = "${pkgs.alacritty}/bin/alacritty";
  #terminal-bin = "${pkgs.kitty}/bin/kitty";
  #terminal-bin = "${pkgs.wezterm}/bin/wezterm start --always-new-process";
  # dev-env = name:
  #   pkgs.writeShellApplication {
  #     inherit name;
  #     runtimeInputs = with pkgs; [wezterm];
  #     text = ''
  #       exec wezterm connect --class=${name} ${name}
  #     '';
  #   };

  # dev-env = {
  #   name,
  #   host ? null,
  # }:
  #   pkgs.writeShellApplication {
  #     inherit name;
  #     runtimeInputs = with pkgs; [kitty];
  #     text = ''
  #       ${
  #         if host == null
  #         then ''
  #           exec kitty -1 --instance-group=${name} --class=${name}
  #         ''
  #         else ''
  #           exec kitty -1 --instance-group=${name} --class=${name} kitten ssh ${host}
  #         ''
  #       }
  #     '';
  #   };

  dev-env = {
    name,
    host ? null,
  }:
    pkgs.writeShellApplication {
      inherit name;
      runtimeInputs = with pkgs; [alacritty hyprland-unstable jq];
      text = ''
        PID="$(hyprctl clients -j | jq '[.[] | select(.class == "${name}" and .initialTitle == "Alacritty")] | first | .pid')"
        if [ "$PID" != "null" ]; then
          exec hyprctl dispatch focuswindow "pid:$PID"
        fi
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

  # local-dev = dev-env "local-dev";
  # remote-dev = dev-env "remote-dev";

  local-dev = dev-env {name = "local-dev";};
  remote-dev = dev-env {
    name = "remote-dev";
    host = config.userinfo.devRemote;
  };
in {
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
    monitor = ",highres,auto,1";
    bind =
      [
        "$mod, Return, exec, ${terminal-bin}"
        "$mod SHIFT, q, killactive"
        "$mod, d, exec, ${pkgs.rofi-wayland}/bin/rofi -show combi -modes combi -combi-modes \"drun,run\""
        "$mod SHIFT, e, exec, ${local-dev}/bin/local-dev"
        "$mod SHIFT, r, exec, ${remote-dev}/bin/remote-dev"
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
        "$mod, space, exec, ${swapCycleNext}/bin/swap-next"
        "$mod SHIFT, space, exec, ${swapCyclePrev}/bin/swap-prev"
        "$mod CONTROL, space, layoutmsg, swapwithmaster"
        "$mod, f, fullscreen"
        "$mod SHIFT, f, fakefullscreen"
      ];

    group = {
      groupbar = {
        font_size = 12;
        gradients = false;
        "col.inactive" = "0x2E344000";
        "col.active" = "0x5E81AC00";
      };
    };

    misc.disable_hyprland_logo = true;
    misc.disable_splash_rendering = true;

    binds = {
      workspace_back_and_forth = true;
      allow_workspace_cycles = true;
    };

    animations = {
      enabled = true;
      animation = [
        "workspaces,1,2,default,slidefade 10%"
        "windows,1,1,default"
        "fade,1,5,default"
        "border,1,1,default"
        "borderangle,1,1,default"
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
    };

    decoration = {
      rounding = 12;
      blur = {
        enabled = true;
        size = 9;
        passes = 4;
        xray = true;
        ignore_opacity = true;
        new_optimizations = true;
        noise = 0.02;
        contrast = 1.05;
        brightness = 1.2;
      };
      drop_shadow = true;
      shadow_range = 20;
      shadow_render_power = 2;
      shadow_offset = "3 3";
      "col.shadow" = "0x99000000";
      "col.shadow_inactive" = "0x55000000";
      active_opacity = 0.95;
      inactive_opacity = 0.87;
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

    # layerrule = ["blur,waybar"];

    windowrulev2 = [
      "dimaround,class:gcr-prompter"
      "stayfocused,class:gcr-prompter"
      "dimaround,class:gitui"
      "stayfocused,class:gitui"
      "float,class:gitui"
      "size 60% 60%,class:gitui"
      "center,class:gitui"
    ];

    input = {
      kb_layout = "us,se";
      kb_variant = "";
      kb_model = "pc105";
      kb_options = "ctrl:nocaps,grp:switch,compose:rctrl";
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
      "${swayidleCommand}/bin/swayidle"
    ];
  };
}
