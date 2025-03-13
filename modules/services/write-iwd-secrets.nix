{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.services.write-iwd-secrets;
in {
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
        SECRETS=/run/agenix/wifi-networks
        mkdir -p /var/lib/iwd
        if [ ! -e "$SECRETS" ]; then
          echo "WARNING - no wifi network secrets at $SECRETS"
          exit 0
        fi
        OIFS="$IFS"
        IFS=$'\n'
        for key in $(jq -r ". | keys | .[]" "$SECRETS"); do
            file="$key"
            fileext="''${key##*.}"
            SSID="$(basename "$key" .$fileext)"
            if echo -n "$SSID" | grep -vq '^[a-zA-Z_0-9-]' >/dev/null; then
              file="=$(echo -n "$SSID" | od -A n -t x1 | sed 's| *||g').$fileext"
            fi
            echo Writing wifi network secrets to /var/lib/iwd/"$file"
            cat<<EOF>/var/lib/iwd/"$file"
        [Security]
        EOF
            for field in $(jq -r ".\"$key\" | keys | .[] | select(. != \"Band\")" "$SECRETS"); do
                value=$(jq -r ".\"$key\".\"$field\"" "$SECRETS")
                cat<<EOF>>/var/lib/iwd/"$file"
        $field=$value
        EOF
            done
            if jq -e ".\"$key\" | keys | any(. == \"Band\")" "$SECRETS" > /dev/null; then
              cat<<EOF>>/var/lib/iwd/"$file"
        [Settings]
        Band=$(jq -r ".\"$key\" | .Band" "$SECRETS")
        EOF
            fi
        done
      '';
      wantedBy = ["network.target"];
    };
  };
}
