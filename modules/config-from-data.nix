{
  hostConfiguration,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (builtins) hasAttr isAttrs isString mapAttrs attrNames length tail head;
  inherit (lib) mkIf concatStringsSep filterAttrs recursiveUpdate hasPrefix splitString last;

  cfgMapper = {
    "age.secrets" = mapAttrs (
      _: value:
        value // {file = ../. + "/${value.file}";}
    );
    "users.users" = mapAttrs (
      _: value:
        value
        // {
          shell =
            if hasAttr "shell" value
            then pkgs.${value.shell}
            else pkgs.bash;
          extraGroups =
            if hasAttr "extraGroups" value
            then value.extraGroups
            else ["wheel" "docker" "video" "audio" "kvm" "libvirtd"];
        }
        // (
          if hasAttr "isSystemUser" value
          then {}
          else {isNormalUser = true;}
        )
    );
  };
  mapString = str:
    if hasPrefix "pkg:" str
    then let
      spec = last (splitString "pkg:" str);
      path = tail (splitString "/" spec);
      pkg = head (splitString "/" spec);
    in
      if (length path) == 0
      then pkgs.${pkg}
      else concatStringsSep "/" (["${pkgs.${pkg}}"] ++ path)
    else str;
  mapConfig = path:
    mapAttrs (
      name: value: let
        stringPath = concatStringsSep "." (path ++ [name]);
      in
        if path != [] && hasAttr stringPath cfgMapper
        then cfgMapper.${stringPath} value
        else if isAttrs value
        then mapConfig (path ++ [name]) value
        else if isString value
        then mapString value
        else value
    );

  hasState =
    hasAttr "state" config.environment
    && (length (attrNames config.environment.state)) > 0;
  hasSecrets = config.age.secrets != {};
in {
  config = recursiveUpdate (mapConfig [] hostConfiguration) {
    nix.settings.trusted-users =
      attrNames
      (filterAttrs (_: user: user.isNormalUser) config.users.users);

    system.activationScripts.agenixRoot = mkIf (hasSecrets && hasState) {deps = ["stateSetup"];};

    system.activationScripts.agenix = mkIf (hasSecrets && hasState) {deps = ["stateSetup"];};
  };
}
