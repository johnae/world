{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) types;
  inherit (config.services.tailscale.auth) enable;
  cfg = config.services.cloudflare-tailscale-dns;
in {
  options.services.cloudflare-tailscale-dns = lib.mkOption {
    type = types.attrsOf (types.submodule ({name, ...}: {
      options = {
        enable = lib.mkEnableOption "Enable this cloudflare dns tailscale mapping";
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
        cloudflareEnvFile = lib.mkOption {
          type = types.path;
        };
      };
    }));
    default = {};
  };

  config.systemd.services =
    lib.mapAttrs' (name: value: {
      name = "cloudflare-tailscale-dns-${name}.${value.zone}";
      value = {
        description = "Cloudflare dns records mapping ${name}.${value.zone} to host tailscale ip";
        inherit enable;
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = "yes";
          EnvironmentFile = [
            value.cloudflareEnvFile
          ];
        };
        script = let
          fqdn = "${value.name}.${value.zone}";
          api = "https://api.cloudflare.com/client/v4";
          curl = "${pkgs.curl}/bin/curl";
          jq = "${pkgs.jq}/bin/jq";
          tailscale = "${pkgs.tailscale}/bin/tailscale";
          ipFilter =
            if value.host == null
            then ".Self.TailscaleIPs[]"
            else ''.Peer[] | select(.DNSName | startswith("${value.host}.")) | .TailscaleIPs[]'';
        in ''
          set -euo pipefail

          cf() {
            local method="$1"
            local path="$2"
            shift 2
            ${curl} -sS -X "$method" \
              -H "Authorization: Bearer $CF_API_TOKEN" \
              -H "Content-Type: application/json" \
              "${api}$path" "$@" \
              | ${jq} -e 'if .success then . else (.errors | tojson | halt_error(1)) end'
          }

          zone_id=$(cf GET "/zones?name=${value.zone}" | ${jq} -er '.result[0].id')

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

            ## Look up by name AND type. flarectl matched on name alone, so once
            ## an A record existed it could never create the matching AAAA.
            record_id=$(cf GET "/zones/$zone_id/dns_records?name=${fqdn}&type=$type" \
              | ${jq} -r '.result[0].id // ""')

            body=$(${jq} -nc --arg name "${fqdn}" --arg type "$type" --arg content "$ip" \
              '{name: $name, type: $type, content: $content, ttl: 60, proxied: false}')

            if [ -n "$record_id" ]; then
              cf PATCH "/zones/$zone_id/dns_records/$record_id" --data "$body" >/dev/null
            else
              cf POST "/zones/$zone_id/dns_records" --data "$body" >/dev/null
            fi
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
