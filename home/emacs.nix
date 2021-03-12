{ pkgs, config, lib, options, ... }:

let
  home = config.home;
in
{
  services.emacs = {
    enable = true;
    package = pkgs.my-emacs;
    socketActivation.enable = true;
  };

  services.emacs-gcal-sync = {
    enable = true;
    outputDirectory = "/home/${home.username}/.gcal-org-sync";
    calendars = {
      home = "${pkgs.pass}/bin/pass show gcal/home";
      work = "${pkgs.pass}/bin/pass show gcal/work";
    };
  };

  systemd.user.services.org-agenda-sync =
    let
      agenda-sync = pkgs.writeStrictShellScriptBin "agenda-sync" ''
        cd ~/Development/org-agenda
        echo Changes detected in "$(pwd)"
        git add . || true
        git commit -m "Auto-commit" || true
        git pull || true
        git push || true
        git status || true
      '';
    in
    {
      Unit.Description = "Continuously git commit push/pull org agenda";
      Service.ExecStart = "${agenda-sync}/bin/agenda-sync";
    };

  systemd.user.paths.org-agenda-sync = {
    Unit.Description = "Continuously git commit push/pull org agenda";
    Path = {
      PathChanged = "/home/${home.username}/Development/org-agenda";
      Unit = "org-agenda-sync.service";
    };
    Install.WantedBy = [ "paths.target" ];
  };

}
