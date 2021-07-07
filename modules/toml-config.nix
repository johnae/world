{hostConfig, config, lib, ...}:

let
  inherit (builtins) hasAttr mapAttrs attrNames length;
  inherit (lib) mkIf stringAfter;
  secrets = if hasAttr "age" hostConfig then
    {
      age.secrets = mapAttrs (n: v:
        v // { file = ../. + "/${v.file}"; }
      ) hostConfig.age.secrets;
    }
  else {};
  hasState = hasAttr "state" config.environment &&
             (length (attrNames config.environment.state)) > 0;
  hasSecrets = config.age.secrets != {};

  networking = if hasAttr "networking" hostConfig then { inherit (hostConfig) networking; } else {};

  services = if hasAttr "services" hostConfig then { inherit (hostConfig) services; } else {};
in

{
  imports = [ secrets networking services ];


  system.activationScripts.agenixRoot = mkIf (hasSecrets && hasState) { deps = [ "stateSetup" ]; };

  system.activationScripts.agenix = mkIf (hasSecrets && hasState) { deps = [ "stateSetup" ]; };

}
