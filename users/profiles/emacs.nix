{ config, pkgs, ... }:

let
  home = config.home;
in
{
  services.emacs = {
    enable = true;
    package = pkgs.my-emacs;
    socketActivation.enable = true;
  };

  systemd.user.services.org-agenda-sync =
    let
      agenda-sync = pkgs.writeStrictShellScriptBin "agenda-sync" ''
        ssh_identity="/run/secrets/id_ed25519_agenda_updater"
        if [ ! -e "$ssh_identity" ]; then
          echo "$ssh_identity" missing
          exit 1
        fi
        GIT_SSH_COMMAND="ssh -i $ssh_identity -o IdentitiesOnly=yes"
        export GIT_SSH_COMMAND

        if [ ! -d ~/Development/org-agenda ]; then
          cd ~/Development
          git clone git@github.com:johnae/org-agenda
          cd ~
        fi

        echo Starting agenda git sync
        cd ~/Development/org-agenda
        git add .
        git commit -m "Auto-commit" || true
        echo Pulling changes from remote
        git pull
        echo Pushing changes to remote
        git push
        git status
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

  systemd.user.timers.org-agenda-sync = {
    Unit.Description = "Continuously git commit push/pull org agenda";
    Timer = {
      OnCalendar = "*:0/5";
      Unit = "org-agenda-sync.service";
    };
    Install.WantedBy = [ "timers.target" ];
  };

}
