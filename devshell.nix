{ mkDevShell, agenix, age-plugin-yubikey, rage, worldUtils, lib }:

let
  worldUtilsList = lib.mapAttrsToList (_: util: util) worldUtils;
in

mkDevShell {
  name = "world";
  packages = [ rage agenix age-plugin-yubikey ] ++ worldUtilsList;
  intro = ''

    Hello, world!

  '';
}
