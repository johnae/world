{
  pkgs,
  config,
  lib,
  ...
}: let
  restic-pkgs =
    lib.mapAttrsToList (
      name: value:
        pkgs.writeShellApplication {
          name = "restic-${name}";
          runtimeInputs = [pkgs.restic];
          text = ''
            while read -r line;
            do
              eval "export $line"
            done < ${value.environmentFile}
            export RESTIC_PASSWORD_FILE=${value.passwordFile}
            export RESTIC_REPOSITORY=${value.repository}

            restic "$@"
          '';
        }
    )
    config.services.restic.backups;
in {
  environment.systemPackages = restic-pkgs;
}
