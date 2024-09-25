{
  pkgs,
  lib,
  ...
}: let
  runViaSystemdCat = {
    name,
    cmd,
    systemdSession,
  }:
    pkgs.writeShellApplication {
      inherit name;
      text = ''
        trap 'systemctl --user stop ${systemdSession} || true' EXIT
        ${pkgs.systemd}/bin/systemd-cat --identifier=${name} ${cmd}
      '';
    };

  runViaShell = {
    env ? {},
    sourceHmVars ? true,
    viaSystemdCat ? true,
    name,
    cmd,
  }:
    pkgs.writeShellApplication {
      inherit name;
      text = ''
        ${lib.concatStringsSep "\n" (lib.mapAttrsToList (k: v: "export ${k}=\"${v}\"") env)}
        ${
          if sourceHmVars
          then ''
            if [ -e /etc/profiles/per-user/"$USER"/etc/profile.d/hm-session-vars.sh ]; then
              set +u
              # shellcheck disable=SC1090
              source /etc/profiles/per-user/"$USER"/etc/profile.d/hm-session-vars.sh
              set -u
            fi
          ''
          else ""
        }
        ${
          if viaSystemdCat
          then ''
            exec ${runViaSystemdCat {
              inherit name cmd;
              systemdSession = "${lib.toLower name}-session.target";
            }}/bin/${name}
          ''
          else ''
            exec ${cmd}
          ''
        }
      '';
    };

  runSway = runViaShell {
    env = {
      XDG_SESSION_TYPE = "wayland";
      XDG_CURRENT_DESKTOP = "sway";
      XDG_SESSION_DESKTOP = "sway";
    };
    name = "sway";
    cmd = "${pkgs.sway}/bin/sway";
  };

  runRiver = runViaShell {
    env = {
      XDG_SESSION_TYPE = "wayland";
      XDG_CURRENT_DESKTOP = "river";
      XDG_SESSION_DESKTOP = "river";
    };
    name = "river";
    cmd = "${pkgs.river}/bin/river";
  };

  desktopSession = name: command:
    pkgs.writeText "${name}.desktop" ''
      [Desktop Entry]
      Type=Application
      Name=${name}
      Exec=${command}
    '';

  sessions = [
    {
      name = "wayland-sessions/sway.desktop";
      path = desktopSession "sway" "${runSway}/bin/sway";
    }
    {
      name = "wayland-sessions/river.desktop";
      path = desktopSession "river" "${runRiver}/bin/river";
    }
    {
      name = "wayland-sessions/nushell.desktop";
      path = desktopSession "nushell" "${pkgs.nushell}/bin/nu";
    }
    {
      name = "wayland-sessions/bash.desktop";
      path = desktopSession "bash" "${pkgs.bashInteractive}/bin/bash";
    }
  ];

  kanshiConf = let
    conf = import ../users/profiles/kanshi.nix;
  in
    pkgs.writeText "kanshi-conf" (
      builtins.concatStringsSep "\n" (
        map (setting: ''
          profile ${setting.profile.name} {
          ${builtins.concatStringsSep "\n" (map (output: ''output "${output.criteria}" mode ${output.mode} position ${output.position} scale ${toString output.scale}'') setting.profile.outputs)}
          }
        '')
        conf.services.kanshi.settings
      )
    );

  regreetCss = pkgs.writeText "regreet-css" ''
    button,
    entry,
    infobar.error > revealer > box {
      background-color: transparent;
      background-image: none;
    }

    frame {
      /* background: transparent !important; */
      /* background: alpha(black, 0.4) !important; */
      /* background-color: alpha(black, 0.4) !important; */
      box-shadow: 0px 0px 8px 0px black;
      border: none;
    }
  '';

  swayConfig = let
    conf = pkgs.callPackage ../users/profiles/sway.nix {};
  in
    pkgs.writeText "sway-config" ''
      exec dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY SWAYSOCK
      exec ${pkgs.gnome-settings-daemon}/libexec/gsd-xsettings
      exec ${pkgs.kanshi}/bin/kanshi -c ${kanshiConf}
      output * bg ${../files/background.jpg} fill
      exec "regreet -s ${regreetCss} ; swaymsg exit"
      for_window [title=".*"] move container to output left
      ${builtins.concatStringsSep "\n" (
        lib.mapAttrsToList (name: value: ''
          input "${name}" {
          ${builtins.concatStringsSep "\n" (
            lib.mapAttrsToList (name: value: ''
              ${name} ${value}
            '')
            value
          )}
          }
        '')
        conf.wayland.windowManager.sway.config.input
      )}

      input "1267:12850:ELAN06A1:00_04F3:3232_Touchpad" {
        dwt true
        natural_scroll true
        tap true
      }
    '';

  createGreeter = default: sessions: let
    sessionDir = pkgs.linkFarm "sessions" (
      builtins.filter (item: item.name != "${default}.desktop") sessions
    );
  in
    pkgs.writeShellApplication {
      name = "greeter";
      runtimeInputs = [runSway pkgs.bashInteractive pkgs.nushell pkgs.systemd pkgs.greetd.tuigreet pkgs.greetd.regreet];
      text = ''
        export XDG_DATA_DIRS="${sessionDir}"
        ${pkgs.dbus}/bin/dbus-run-session ${lib.getExe pkgs.sway} -c ${swayConfig}
      '';
    };
in {
  programs.regreet.enable = true;

  environment.systemPackages = [pkgs.nordic pkgs.nordzy-cursor-theme pkgs.arc-icon-theme];

  programs.regreet.settings = {
    background = {
      path = ../files/background.jpg;
      fit = "Cover";
    };
    commands = {
      reboot = ["systemctl" "reboot"];
      poweroff = ["systemctl" "poweroff"];
    };
    appearance = {
      greeting_msg = "Welcome back!";
    };
    GTK = {
      cursor_theme_name = lib.mkForce "Nordzy-cursors";
      font_name = lib.mkForce "Roboto Medium 14";
      icon_theme_name = lib.mkForce "Arc";
      theme_name = lib.mkForce "Nordic-darker";
      application_prefer_dark_theme = lib.mkForce true;
    };
  };
  services.greetd = {
    enable = true;
    restart = true;
    settings = {
      default_session.command = "${createGreeter "${runRiver}/bin/river" sessions}/bin/greeter";
    };
  };
  ## prevents systemd spewing the console with log messages when greeter is active
  systemd.services.greetd.serviceConfig = {
    ExecStartPre = "${pkgs.util-linux}/bin/kill -SIGRTMIN+21 1";
    ExecStopPost = "${pkgs.util-linux}/bin/kill -SIGRTMIN+20 1";
  };
}
