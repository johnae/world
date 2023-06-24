{
  pkgs,
  lib,
  ...
}: let
  runViaSystemdCat = {
    name,
    cmd,
  }:
    pkgs.writeShellApplication {
      inherit name;
      text = ''
        exec ${pkgs.udev}/bin/systemd-cat --identifier=${name} ${cmd}
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
            exec ${runViaSystemdCat {inherit name cmd;}}/bin/${name}
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

  desktopSession = name: command: ''
    [Desktop Entry]
    Type=Application
    Name=${name}
    Exec=${command}
  '';

  sessions = [
    {
      name = "sway";
      value = desktopSession "Sway" "${runSway}/bin/sway";
    }
    {
      name = "river";
      value = desktopSession "River" "${runRiver}/bin/river";
    }
    {
      name = "nushell";
      value = desktopSession "NuShell" "${pkgs.nushell}/bin/nu";
    }
  ];

  createTuiGreeter = default: sessions: let
    sessionDir = pkgs.linkFarm "sessions" (
      map (session: pkgs.writeText "${session.name}" session.value) (builtins.filter (item: item.name != default) sessions)
    );
  in
    pkgs.writeShellApplication {
      name = "greeter";
      runtimeInputs = [runSway runRiver pkgs.nushell pkgs.systemd pkgs.greetd.tuigreet];
      text = ''
        tuigreet --sessions ${sessionDir} --time -r --remember-session --power-shutdown 'systemctl poweroff' --power-reboot 'systemctl reboot' --cmd ${default}
      '';
    };

  regreetBg = "${../files/forest.jpg}";

  createRegreetGreeter = sessions: let
    waylandSessions = map (session: pkgs.writeTextDir "wayland-sessions/${session.name}.desktop" session.value) sessions;
    sessionDir = pkgs.symlinkJoin {
      name = "sessions";
      paths = [waylandSessions];
    };
    swayConfig = pkgs.writeText "sway.config.regreet" ''
      output "*" {
      bg ${regreetBg} fill
      }
      exec "XDG_DATA_DIRS=${sessionDir} regreet -l debug; swaymsg exit"
    '';
  in
    pkgs.writeShellApplication {
      name = "greeter";
      runtimeInputs = [runSway runRiver pkgs.nushell pkgs.systemd pkgs.greetd.regreet pkgs.sway pkgs.swaybg];
      text = ''
        echo "Starting sway-greeter" | tee -a /tmp/sway-greeter.log
        export XDG_DATA_DIRS=${sessionDir}
        export XDG_SESSION_TYPE="wayland"
        export XDG_CURRENT_DESKTOP="sway"
        export XDG_SESSION_DESKTOP="sway"
        ${pkgs.dbus}/bin/dbus-update-activation-environment --systemd --all
        ${pkgs.dbus}/bin/dbus-run-session ${lib.getExe pkgs.sway} -c ${swayConfig} 2>&1 | tee -a /tmp/sway-greeter.log
      '';
    };
in {
  environment.systemPackages = [
    pkgs.nordzy-cursor-theme
    pkgs.nordic
    pkgs.arc-icon-theme
    pkgs.roboto
  ];
  programs.regreet = {
    enable = true;
    settings = {
      background = {
        path = regreetBg;
        fit = "Cover";
      };
      gtk = {
        application_prefer_dark_theme = false;
        cursor_theme = "Nordzy-cursors";
        icon_theme_name = "Arc";
        theme_name = "Nordic-darker";
        font_name = "Roboto Medium 11";
      };
      commands = {
        reboot = ["systemctl" "reboot"];
        poweroff = ["systemctl" "poweroff"];
      };
    };
  };
  services.greetd = {
    enable = true;
    restart = true;
    settings = {
      #default_session.command = "${createTuiGreeter "sway" sessions}/bin/greeter";
      default_session.command = "${createRegreetGreeter sessions}/bin/greeter";
    };
  };
  ## prevents systemd spewing the console with log messages when greeter is active
  systemd.services.greetd.serviceConfig = {
    ExecStartPre = "${pkgs.util-linux}/bin/kill -SIGRTMIN+21 1";
    ExecStopPost = "${pkgs.util-linux}/bin/kill -SIGRTMIN+20 1";
  };
}
