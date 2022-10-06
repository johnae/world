{pkgs, ...}: let
  runSway = pkgs.writeShellApplication {
    name = "run-sway";
    text = ''
      exec ${pkgs.udev}/bin/systemd-cat --identifier=sway sway
    '';
  };
  runSwayViaShell = pkgs.writeShellApplication {
    name = "run-sway-via-shell";
    text = ''
      export XDG_SESSION_TYPE=wayland
      export XDG_CURRENT_DESKTOP=sway
      export XDG_SESSION_DESKTOP=sway
      if [ -e /etc/profiles/per-user/"$USER"/etc/profile.d/hm-session-vars.sh ]; then
        set +u
        # shellcheck disable=SC1090
        source /etc/profiles/per-user/"$USER"/etc/profile.d/hm-session-vars.sh
        set -u
      fi
      exec ${runSway}/bin/run-sway
    '';
  };
in {
  services.greetd = {
    enable = true;
    restart = true;
    settings = {
      default_session.command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd ${runSwayViaShell}/bin/run-sway-via-shell";
    };
  };
  ## prevents systemd spewing the console with log messages when greeter is active
  systemd.services.greetd.serviceConfig = {
    ExecStartPre = "${pkgs.util-linux}/bin/kill -SIGRTMIN+21 1";
    ExecStopPost = "${pkgs.util-linux}/bin/kill -SIGRTMIN+20 1";
  };
}
