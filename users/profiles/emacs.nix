{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (config) home;
  inherit (lib) mkMerge;
  repo-sync = name: key: let
    sync-script = pkgs.writeStrictShellScriptBin "repo-sync-${name}" ''
      ssh_identity="/run/agenix/${key}"
      if [ ! -e "$ssh_identity" ]; then
        echo "$ssh_identity" missing
        exit 1
      fi
      GIT_SSH_COMMAND="ssh -i $ssh_identity -o IdentitiesOnly=yes"
      export GIT_SSH_COMMAND

      if [ ! -d ~/Development/${name} ]; then
        cd ~/Development
        git clone git@github.com:johnae/${name}
        cd ~
      fi

      echo Starting ${name} git sync
      cd ~/Development/${name}
      git add .
      git commit -m "Auto-commit" || true
      echo Pulling changes from remote
      git pull
      echo Pushing changes to remote
      git push
      git status
    '';
  in {
    systemd.user.services."${name}-sync" = {
      Unit.Description = "Continuously git commit push/pull ${name}";
      Service.ExecStart = "${sync-script}/bin/repo-sync-${name}";
    };

    systemd.user.paths."${name}-sync" = {
      Unit.Description = "Continuously git commit push/pull ${name}";
      Path = {
        PathChanged = "/home/${home.username}/Development/${name}";
        Unit = "${name}-sync.service";
      };
      Install.WantedBy = ["paths.target"];
    };

    systemd.user.timers."${name}-sync" = {
      Unit.Description = "Continuously git commit push/pull ${name}";
      Timer = {
        OnCalendar = "*:0/5";
        Unit = "${name}-sync.service";
      };
      Install.WantedBy = ["timers.target"];
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
