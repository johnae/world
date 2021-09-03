{userProfiles, lib, ...}:
let
  inherit (lib) mkOption;
  inherit (lib.types) submodule listOf attrsOf str;
in
{
  options = {
    userConfiguration = mkOption {
      type = attrsOf (submodule ({ name, ...}:
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
}
