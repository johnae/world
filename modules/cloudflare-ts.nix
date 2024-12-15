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
        delete = lib.mkOption {
          type = types.bool;
          default = false;
        };
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
              TS_IP="$(${pkgs.tailscale}/bin/tailscale status --json | ${pkgs.jq}/bin/jq -r '.Self.TailscaleIPs[0]')"
            ''
            else ''
              TS_IP="$(${pkgs.tailscale}/bin/tailscale status --json | ${pkgs.jq}/bin/jq -r '.Peer[] | select(.DNSName | startswith("${value.host}.")) | .TailscaleIPs[0]')"
            ''
          }
          ${
            if value.delete
            then ''
              RECORD_ID="$(${pkgs.flarectl}/bin/flarectl --json dns list --zone 9000.dev | ${pkgs.jq}/bin/jq -r '.[] | select(.Name == "${value.name}.${value.zone}") | .ID')"
              ${pkgs.flarectl}/bin/flarectl dns delete --zone ${value.zone} --id "$RECORD_ID"
            ''
            else ''
              ${pkgs.flarectl}/bin/flarectl dns create-or-update --zone ${value.zone} --name "${value.name}" --type A --ttl 60 --content "$TS_IP"
            ''
          }
        '';
        after = ["network-online.target" "tailscale-auth.service"];
        requires = ["network-online.target" "tailscale-auth.service"];
        wantedBy = ["multi-user.target"];
      };
    })
    cfg;
}
