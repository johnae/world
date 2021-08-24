{lib, config, ...}:
let
  inherit (lib) mkOption mkIf mkMerge mkForce types;
  inherit (builtins) concatStringsSep;
  cfg = config.services.k3s;
in
{
  options.services.k3s.nodeID = mkOption {
    type = types.nullOr types.str;
    default = null;
  };
  options.services.k3s.extraFlagsList = mkOption {
    type = types.listOf types.str;
    default = [];
  };
  config = mkIf (cfg.nodeID != null) {
    systemd.enableUnifiedCgroupHierarchy = mkForce true;
    services.k3s.extraFlagsList = [ "--with-node-id ${cfg.nodeID}" ];
    services.k3s.extraFlags = concatStringsSep " " cfg.extraFlagsList;
  };
}
