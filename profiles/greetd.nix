{pkgs, ...}: {
  services.greetd = {
    enable = true;
    restart = true;
    settings = {
      default_session.command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd sway";
    };
  };
}
