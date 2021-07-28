{ pkgs, ... }:

#let
#  home = config.home;
#in
{
  services.emacs = {
    enable = true;
    package = pkgs.my-emacs;
    socketActivation.enable = true;
  };

  #services.emacs-gcal-sync = {
  #  enable = true;
  #  outputDirectory = "/home/${home.username}/.gcal-org-sync";
  #  calendars = {
  #    home = "${pkgs.pass}/bin/pass show gcal/home";
  #    work = "${pkgs.pass}/bin/pass show gcal/work";
  #  };
  #};

  #systemd.user.services.org-agenda-sync =
  #  let
  #    agenda-sync = pkgs.writeStrictShellScriptBin "agenda-sync" ''
  #      echo Starting agenda git sync
  #      cd ~/Development/org-agenda
  #      git add .
  #      git commit -m "Auto-commit" || true
  #      echo Pulling changes from remote
  #      git pull
  #      echo Pushing changes to remote
  #      git push
  #      git status
  #    '';
  #  in
  #  {
  #    Unit.Description = "Continuously git commit push/pull org agenda";
  #    Service.ExecStart = "${agenda-sync}/bin/agenda-sync";
  #  };

  #systemd.user.paths.org-agenda-sync = {
  #  Unit.Description = "Continuously git commit push/pull org agenda";
  #  Path = {
  #    PathChanged = "/home/${home.username}/Development/org-agenda";
  #    Unit = "org-agenda-sync.service";
  #  };
  #  Install.WantedBy = [ "paths.target" ];
  #};

  #systemd.user.timers.org-agenda-sync = {
  #  Unit.Description = "Continuously git commit push/pull org agenda";
  #  Timer = {
  #    OnCalendar = "*:0/5";
  #    Unit = "org-agenda-sync.service";
  #  };
  #  Install.WantedBy = [ "timers.target" ];
  #};

}
