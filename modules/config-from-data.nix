{
  hostConfiguration,
  config,
  lib,
  pkgs,
  ...
} @ args: let
  inherit (builtins) hasAttr isAttrs isString mapAttrs attrNames length tail head split filter isList;
  inherit (lib) mkIf concatStringsSep filterAttrs recursiveUpdate hasPrefix splitString last attrByPath take last;

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
  ## really just does package interpolation... sort of - needs a bit of further work really
  mapString = str: let
    itemized = s: filter (item: item != "") (split "(\\$\\{[a-zA-Z0-9_-]+\.[a-zA-Z0-9/._-]+})" s);
    pkgref = ref:
      map (
        items: let
          pkg = attrByPath (take 2 items) null args;
          path = last items;
        in
          if path != ""
          then "${pkg}${path}"
          else pkg
      ) (filter isList (split "\\$\\{([a-zA-Z0-9_-]+)\.([a-zA-Z0-9._-]+)(.*)}" ref));
    items = itemized str;
  in
    if length items == 1
    then
      if hasPrefix "\${" str
      then head (pkgref str)
      else str
    else
      concatStringsSep "" (map (
          item:
            if isList item
            then head (pkgref (head item))
            else item
        )
        items);
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
