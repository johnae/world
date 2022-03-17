{pkgs, ...}: {
  systemd.user.services.pueued = {
    Unit.Description = "Pueue Daemon - CLI process scheduler and manager";
    Service = {
      Restart = "no";
      ExecStart = "${pkgs.pueue}/bin/pueued -vv";
    };
    Install.WantedBy = ["default.target"];
  };
}
