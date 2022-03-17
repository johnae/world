{
  userProfiles,
  lib,
  ...
}: let
  inherit (lib) mkOption;
  inherit (lib.types) submodule listOf attrsOf str;
in {
  options = {
    home = mkOption {
      type = attrsOf (submodule ({name, ...}: {
        options = {
          name = mkOption {
            type = str;
            default = name;
          };
          profiles = mkOption {
            type = listOf str;
            apply = map (v: userProfiles.${v});
          };
        };
      }));
      default = {};
    };
  };
}
