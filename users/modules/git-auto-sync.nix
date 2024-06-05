{
  config,
  pkgs,
  lib,
  ...
}: let
  l = builtins // lib;
  inherit
    (l)
    mkOption
    types
    baseNameOf
    dirOf
    mapAttrs'
    nameValuePair
    ;
  cfg = config.services.gitAutoSync;
  repo-sync = {
    repository,
    path,
    sshKey,
  }: let
    name = baseNameOf path;
    parent = dirOf path;
  in
    pkgs.writeShellApplication {
      name = "repo-sync";
      runtimeInputs = with pkgs; [inotify-tools gitMinimal openssh];
      text = ''
        ssh_identity="${sshKey}"
        if [ ! -e "$ssh_identity" ]; then
          echo "$ssh_identity" missing
          exit 1
        fi
        GIT_SSH_COMMAND="ssh -i $ssh_identity -o IdentitiesOnly=yes -o ControlMaster=no -o ControlPath=/dev/null"
        export GIT_SSH_COMMAND

        if [ ! -d ${path} ]; then
          cd ${parent}
          git clone ${repository}
          cd ~
        fi

        echo Starting ${name} git sync
        cd ${path}

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
    };
in {
  options.services.gitAutoSync = {
    repos = mkOption {
      type = types.attrsOf (types.submodule (
        {name, ...}: {
          options.repository = mkOption {
            type = types.str;
            description = "Git repo url";
            example = "git@github.com:me/my";
          };
          options.path = mkOption {
            type = types.str;
            description = "String path to checkout";
            example = "~/Path/to/checkout";
            default = name;
          };
          options.sshKey = mkOption {
            type = types.str;
            description = "String path to ssh key";
            example = "/run/agenix/id_ed25519_some_key";
          };
        }
      ));
      default = {};
    };
  };

  config = {
    systemd.user.services =
      mapAttrs' (
        path: spec: let
          name = baseNameOf path;
          sync-script = repo-sync spec;
        in
          nameValuePair "${name}-git-auto-sync" {
            Unit.Description = "Continuously git commit push/pull ${name}";
            Install.WantedBy = ["default.target"];
            Service = {
              ExecStart = "${sync-script}/bin/repo-sync";
              Restart = "always";
              RestartSec = 3;
            };
          }
      )
      cfg.repos;
  };
}
