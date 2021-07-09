{hostName, hostConfig, userProfiles, config, lib, pkgs, ...}:

let
  inherit (builtins) hasAttr isAttrs mapAttrs attrNames length isString pathExists;
  inherit (lib) mkIf mkOption concatStringsSep filterAttrs;
  inherit (lib.types) submodule listOf attrsOf str;
  cfgMapper = {
    "age.secrets" = mapAttrs (name: value:
      value // { file = ../. + "/${value.file}"; }
    );
    "users.users" = mapAttrs (name: value:
      value // {
        shell = if hasAttr "shell" value then
          pkgs.${value.shell}
        else pkgs.bash;
        extraGroups = if hasAttr "extraGroups" value then
          value.extraGroups
        else [ "wheel" "docker" "video" "audio" "plugdev" ];
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
  options = {
    publicKey = mkOption {
      type = str;
      default = "xxxx";
    };
    userConfiguration = mkOption {
      type = attrsOf (submodule ({ config, name, ...}:
        {
          options = {
            name = mkOption {
              type = str;
              default = name;
            };
            email = mkOption {
              type = str;
              example = "example@example.com";
            };
            fullName = mkOption {
              type = str;
              example = "Recurse Recursson";
            };
            githubUser = mkOption {
              type = str;
            };
            gitlabUser = mkOption {
              type = str;
            };
            profiles = mkOption {
              type = listOf (str);
              apply = map (v: userProfiles.${v});
            };
          };
        }));
      default = {};
    };
  };

  config = (mapConfig [] hostConfig) // {

    nix.trustedUsers = attrNames
      (filterAttrs (_: user: user.isNormalUser) config.users.users);

    system.activationScripts.agenixRoot = mkIf (hasSecrets && hasState) { deps = [ "stateSetup" ]; };

    system.activationScripts.agenix = mkIf (hasSecrets && hasState) { deps = [ "stateSetup" ]; };
  };
}
