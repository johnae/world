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
in

{
  imports = [ secrets ];

  system.activationScripts.agenixRoot.deps = mkIf hasState [ "stateSetup" ];
  system.activationScripts.agenix.deps = mkIf hasState [ "stateSetup" ];
}
