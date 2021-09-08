{hostConfig, config, lib, pkgs, ...}:

let
  inherit (builtins) hasAttr isAttrs mapAttrs attrNames length;
  inherit (lib) mkIf concatStringsSep filterAttrs recursiveUpdate;

  cfgMapper = {
    "age.secrets" = mapAttrs (_: value:
      value // { file = ../. + "/${value.file}"; }
    );
    "users.users" = mapAttrs (_: value:
      value // {
        shell = if hasAttr "shell" value then
          pkgs.${value.shell}
        else pkgs.bash;
        extraGroups = if hasAttr "extraGroups" value then
          value.extraGroups
        else [ "wheel" "docker" "video" "audio" "kvm" "libvirtd" ];
        isNormalUser = true;
      }
    );
  };
  mapConfig = path: mapAttrs (name: value:
    let
      stringPath = concatStringsSep "." (path ++ [ name ]);
    in
    if path != [] && hasAttr stringPath cfgMapper then
      cfgMapper.${stringPath} value
    else if isAttrs value then
      mapConfig (path ++ [name]) value
    else value
  );

  hasState = hasAttr "state" config.environment &&
             (length (attrNames config.environment.state)) > 0;
  hasSecrets = config.age.secrets != {};
in
{
  config = recursiveUpdate (mapConfig [] hostConfig) {

    nix.trustedUsers = attrNames
      (filterAttrs (_: user: user.isNormalUser) config.users.users);

    system.activationScripts.agenixRoot = mkIf (hasSecrets && hasState) { deps = [ "stateSetup" ]; };

    system.activationScripts.agenix = mkIf (hasSecrets && hasState) { deps = [ "stateSetup" ]; };
  };
}
