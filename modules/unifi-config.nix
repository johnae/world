{lib, config, pkgs, utils, ...}:

### this extends the existing unifi module

let
  inherit (lib) mapAttrsToList mkIf mkOption types mkMerge;
  inherit (builtins) toJSON;
  cfg = config.services.unifi;
  dataDir = cfg.dataDir;
  sitesDir = "${dataDir}/sites";
  configFile = config: pkgs.writeText "config.gateway.json"
    (toJSON config);
  mountPoints = mapAttrsToList (_: conf:
    {
      what = "${configFile conf.config}";
      where = "${sitesDir}/${conf.site}/config.gateway.json";
    }
  ) cfg.configGateway;
  systemdMountPoints = map (m: "${utils.escapeSystemdPath m.where}") mountPoints;
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
    systemd.services = map ({what, where}: {
      bindsTo = [ "unifi.service" ];
      partOf = [ "unifi.service" ];
      unitConfig.RequiresMountsFor = dataDir;
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = "yes";
      };
      script = ''
        cp ${what} ${where}
      '';
      enable = true;
    }) mountPoints;
    systemd.services.unifi = {
      after = systemdMountPoints;
      partOf = systemdMountPoints;
      bindsTo = systemdMountPoints;
    };
  };
}
