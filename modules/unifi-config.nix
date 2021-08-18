{lib, config, pkgs, utils, ...}:

### this extends the existing unifi module

let
  inherit (lib) mapAttrsToList mkIf mkOption types mkMerge listToAttrs;
  inherit (builtins) toJSON;
  cfg = config.services.unifi;
  dataDir = cfg.dataDir;
  sitesDir = "${dataDir}/sites";
  configFile = config: pkgs.writeText "config.gateway.json"
    (toJSON config);
  configDestinations = mapAttrsToList (_: conf:
    {
      what = "${configFile conf.config}";
      where = "${sitesDir}/${conf.site}/config.gateway.json";
    }
  ) cfg.configGateway;
  systemdCopyServices = map (m: "${utils.escapeSystemdPath m.where}.service") configDestinations;
in
{
  options = {
    services.unifi.configGateway = mkOption {
      type = types.attrsOf (types.submodule ({ name, ...}:
        {
          options = {
            site = mkOption {
              type = types.str;
              default = name;
            };
            config = mkOption {
              type = types.attrs;
              description = ''
                Content for config.gateway.json
              '';
            };
          };
        }
      ));
      default = {};
    };
  };

  config = mkIf (cfg.enable) {
    systemd.services = (listToAttrs (map ({what, where}: {
      name = utils.escapeSystemdPath where;
      value = {
        bindsTo = [ "unifi.service" ];
        partOf = [ "unifi.service" ];
        unitConfig.RequiresMountsFor = dataDir;
        serviceConfig = {
          User = "unifi";
          UMask = "0077";
          Type = "oneshot";
          RemainAfterExit = "yes";
        };
        script = ''
          mkdir -p "$(dirname "${where}")"
          cp ${what} ${where}
        '';
        enable = true;
      };
    }) configDestinations)) // {
      unifi = {
        after = systemdCopyServices;
        partOf = systemdCopyServices;
        bindsTo = systemdCopyServices;
      };
    };
  };
}
