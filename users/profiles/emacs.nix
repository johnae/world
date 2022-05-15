{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (config) home;
  inherit (lib) mkMerge;
  ## it used to be a service unit, a path unit and a timer unit but that doesn't
  ## work well when new files are added or when keeping files in a tree structure
  ## - path units aren't recursvide and don't detect new files/folders
  repo-sync = name: key: let
    sync-script = pkgs.writeStrictShellScriptBin "repo-sync-${name}" ''
      export PATH=${pkgs.inotifyTools}/bin:${pkgs.gitMinimal}/bin:${pkgs.openssh}/bin:$PATH
      ssh_identity="/run/agenix/${key}"
      if [ ! -e "$ssh_identity" ]; then
        echo "$ssh_identity" missing
        exit 1
      fi
      GIT_SSH_COMMAND="ssh -i $ssh_identity -o IdentitiesOnly=yes -o ControlMaster=no -o ControlPath=/dev/null"
      export GIT_SSH_COMMAND

      if [ ! -d ~/Development/${name} ]; then
        cd ~/Development
        git clone git@github.com:johnae/${name}
        cd ~
      fi

      echo Starting ${name} git sync
      cd ~/Development/${name}

      while inotifywait -e create,move_self,attrib,delete,moved_to,close_write,delete_self,moved_from,modify,move -t 300 -r .; do
        echo Changes detected
        git add -A .
        if ! git diff --staged --quiet; then
          echo Committing changes
          git commit -m "Auto-commit"
        else
          echo No changes to commit
        fi
        echo Pulling changes from remote
        git pull
        echo Pushing changes to remote
        git push
        git status
      done
    '';
  in {
    systemd.user.services."${name}-sync" = {
      Unit.Description = "Continuously git commit push/pull ${name}";
      Install.WantedBy = ["default.target"];

      Service = {
        ExecStart = "${sync-script}/bin/repo-sync-${name}";
        Restart = "always";
        RestartSec = 3;
      };
    };
  };
in
  mkMerge [
    {
      services.emacs = {
        enable = true;
        package = pkgs.my-emacs;
        socketActivation.enable = true;
      };
    }
    #(
    #  repo-sync "org-agenda" "id_ed25519_agenda_updater"
    #)
    (
      repo-sync "org-roam" "id_ed25519_roam_updater"
    )
  ]
