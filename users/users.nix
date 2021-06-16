{userProfiles, users, pkgs, lib, config, ...}:

let
  #userConfigurations = lib.mapAttrsToList (n: v:
  #  if builtins.pathExists ./. + "/${n}" then
  #    ./. + "/${n}"
  #  else {}
  #) users.users;

  u = lib.mapAttrs (n: v:
    if n == "users" then
      lib.mapAttrs (un: uv:
        if uv.shell then
          { shell = pkgs.${uv.shell}; } // uv
        else uv
      )
    else v
  ) users;
in
{
  imports = [
    users
  ];
}
