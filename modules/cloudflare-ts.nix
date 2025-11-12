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
        script = ''
          ${
            if value.host == null
            then ''
              for IP in $(${pkgs.tailscale}/bin/tailscale status --json | ${pkgs.jq}/bin/jq -r '.Self.TailscaleIPs[]'); do
            ''
            else ''
              for IP in $(${pkgs.tailscale}/bin/tailscale status --json | ${pkgs.jq}/bin/jq -r '.Peer[] | select(.DNSName | startswith("${value.host}.")) | .TailscaleIPs[]'); do
            ''
          }
            if echo "$IP" | grep -q '^f'; then
              ${pkgs.flarectl}/bin/flarectl dns create-or-update --zone ${value.zone} --name "${value.name}" --type AAAA --ttl 60 --content "$IP"
            else
              ${pkgs.flarectl}/bin/flarectl dns create-or-update --zone ${value.zone} --name "${value.name}" --type A --ttl 60 --content "$IP"
            fi
          done
        '';
        after = ["network-online.target" "tailscale-auth.service"];
        requires = ["network-online.target" "tailscale-auth.service"];
        wantedBy = ["multi-user.target"];
      };
    })
    cfg;
}
