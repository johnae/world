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
    settings = mkOption {
      type = settingsFormat.type;
      default = {};
      description = ''
        Configuration for headscale acls
      '';
    };
  };

  config = mkIf cfg.enable {
    services.headscale.settings.acl_policy_path = settingsFormat.generate "headscale-acls.json" cfg.settings;
  };
}
