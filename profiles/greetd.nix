{pkgs, ...}: {
  services.greetd = {
    enable = true;
    restart = true;
    settings = {
      default_session.command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd sway";
    };
  };
  ## prevents systemd spewing the console with log messages when greeter is active
  systemd.services.greetd.serviceConfig = {
    ExecStartPre = "${pkgs.util-linux}/bin/kill -SIGRTMIN+21 1";
    ExecStopPost = "${pkgs.util-linux}/bin/kill -SIGRTMIN+20 1";
  };
}
