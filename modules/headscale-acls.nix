{
  config,
  options,
  pkgs,
  lib,
  ...
}: let
  inherit
    (lib // builtins)
    types
    mkOption
    mkEnableOption
    mkIf
    concatStringsSep
    mapAttrsToList
    isBool
    isList
    toString
    mapAttrs
    ;

  cfg = config.services.headscale.acl;
  settingsFormat = pkgs.formats.json {};
in {
  options.services.headscale.acl = {
    enable = mkEnableOption "enable headscale acl management";
    enableSSH = mkEnableOption "enable headscale experimental ssh support";
    settings = mkOption {
      inherit (settingsFormat) type;
      default = {};
      description = ''
        Configuration for headscale acls
      '';
    };
  };

  config = mkIf cfg.enable {
    services.headscale.settings.acl_policy_path = settingsFormat.generate "headscale-acls.json" cfg.settings;
    systemd.services.headscale.environment = mkIf cfg.enableSSH {
      HEADSCALE_EXPERIMENTAL_FEATURE_SSH = "1";
    };
  };
}
