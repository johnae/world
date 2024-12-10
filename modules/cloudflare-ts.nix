{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) types;
  cfg = config.services.cloudflare-tailscale-dns;
  tsAuthCfg = config.services.tailscale.auth;
in {
  options.services.cloudflare-tailscale-dns = lib.mkOption {
    type = lib.attrsOf (lib.submodule ({name, ...}: {
      options = {
        enable = lib.mkEnableOption "Enable this cloudflare dns tailscale mapping";
        delete = lib.mkOption {
          type = lib.bool;
          default = false;
        };
        name = mkOption {
          type = str;
          default = name;
        };
        zone = mkOption {
          type = str;
        };
        cloudflareEnvFile = lib.mkOption {
          type = types.path;
        };
      };
    }));
    default = {};
  };

  config = lib.mkMerge (lib.mapAttrsToList (
      name: cfg: {
        systemd.services."cloudflare-tailscale-dns-${name}.${cfg.zone}" = {
          description = "Cloudflare dns records mapping ${name}.${cfg.zone} to host tailscale ip";
          inherit (tsAuthCfg) enable;
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = "yes";
            EnvironmentFile = [
              cfg.cloudflareEnvFile
            ];
          };
          script = ''
            TS_IP="$(${pkgs.tailscale}/bin/tailscale status --json | ${pkgs.jq}/bin/jq -r '.Self.TailscaleIPs[0]')"
            ${pkgs.flarectl}/bin/flarectl dns create-or-update --zone ${cfg.zone} --name "${cfg.name}" --type A --ttl 60 --content "$TS_IP"
          '';
          after = ["network-online.target" "tailscale-auth.service"];
          requires = ["network-online.target" "tailscale-auth.service"];
          wantedBy = ["multi-user.target"];
        };
      }
    )
    cfg.cloudflare-tailscale-dns);
}
