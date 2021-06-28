{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.write-iwd-secrets;
in
{
  options.services.write-iwd-secrets = {
    enable = mkEnableOption "write iwd wifi network secrets";
  };

  config = mkIf cfg.enable {
    systemd.services.write-iwd-secrets = {
      description = "Write IWD wifi secrets";
      enable = true;
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = "yes";
      };
      script = ''
        export PATH=${pkgs.jq}/bin''${PATH:+:}$PATH
        umask u=rw,g=,o=
        SECRETS=/run/secrets/wifi-networks
        if [ ! -e "$SECRETS" ]; then
          echo "WARNING - no wifi network secrets at $SECRETS"
          exit 0
        fi
        OIFS="$IFS"
        IFS=$'\n'
        for file in $(jq -r ". | keys | .[]" "$SECRETS"); do
            PreSharedKey="$(jq -r ".\"$file\".PreSharedKey" "$SECRETS")"
            Passphrase="$(jq -r ".\"$file\".Passphrase" "$SECRETS")"
            echo Writing wifi network secrets to /var/lib/iwd/"$file"
            cat<<EOF>/var/lib/iwd/"$file"
        [Security]
        PreSharedKey=$PreSharedKey
        Passphrase=$Passphrase
        EOF
        done
        IFS="$OIFS"
      '';
      wantedBy = [ "network.target" ];
    };
  };
}
