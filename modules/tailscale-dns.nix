{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) types;
  inherit (config.services.tailscale.auth) enable;
  cfg = config.services.tailscale-dns;
in {
  options.services.tailscale-dns = lib.mkOption {
    type = types.attrsOf (types.submodule ({name, ...}: {
      options = {
        enable = lib.mkEnableOption "Enable this Scaleway DNS tailscale mapping";
        name = lib.mkOption {
          type = types.str;
          default = name;
        };
        zone = lib.mkOption {
          type = types.str;
        };
        host = lib.mkOption {
          type = types.nullOr types.str;
          default = null;
        };
        environmentFile = lib.mkOption {
          type = types.path;
          description = "Env file with SCW_SECRET_KEY for a key allowed to write the zone.";
        };
      };
    }));
    default = {};
  };

  config.systemd.services =
    lib.mapAttrs' (name: value: {
      name = "tailscale-dns-${name}.${value.zone}";
      value = {
        description = "Scaleway dns records mapping ${name}.${value.zone} to host tailscale ip";
        inherit enable;
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = "yes";
          EnvironmentFile = [
            value.environmentFile
          ];
        };
        script = let
          fqdn = "${value.name}.${value.zone}";
          api = "https://api.scaleway.com/domain/v2beta1/dns-zones/${value.zone}/records";
          curl = "${pkgs.curl}/bin/curl";
          jq = "${pkgs.jq}/bin/jq";
          tailscale = "${pkgs.tailscale}/bin/tailscale";
          ipFilter =
            if value.host == null
            then ".Self.TailscaleIPs[]"
            else ''.Peer[] | select(.DNSName | startswith("${value.host}.")) | .TailscaleIPs[]'';
        in ''
          set -euo pipefail

          ## Assigned first rather than inlined into the `for` list, where a
          ## tailscale failure would leave the loop empty and the unit green.
          ips=$(${tailscale} status --json | ${jq} -r '${ipFilter}')
          if [ -z "$ips" ]; then
            echo "no tailscale addresses found for ${fqdn}" >&2
            exit 1
          fi

          for ip in $ips; do
            case "$ip" in
              *:*) type=AAAA ;;
              *) type=A ;;
            esac

            ## `set` replaces every record with this name and type, creating
            ## one when there is none, so the call is the same on every boot.
            body=$(${jq} -nc --arg name "${value.name}" --arg type "$type" --arg data "$ip" \
              '{return_all_records: false, changes: [{set: {
                 id_fields: {name: $name, type: $type},
                 records: [{name: $name, type: $type, data: $data, ttl: 60}]}}]}')

            ${curl} -sSf -X PATCH \
              -H "X-Auth-Token: $SCW_SECRET_KEY" \
              -H "Content-Type: application/json" \
              "${api}" --data "$body" >/dev/null
            echo "$type ${fqdn} -> $ip"
          done
        '';
        after = ["network-online.target" "tailscale-auth.service"];
        requires = ["network-online.target" "tailscale-auth.service"];
        wantedBy = ["multi-user.target"];
      };
    })
    cfg;
}
