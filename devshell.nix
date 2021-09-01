{ mkDevShell, agenix, age-plugin-yubikey, rage, yj, worldUtils, lib }:

let
  worldUtilsList = lib.mapAttrsToList (_: util: util) worldUtils;
in

mkDevShell {
  name = "world";
  packages = [ yj rage agenix age-plugin-yubikey ] ++ worldUtilsList;
  intro = ''

    Hello, world!

  '';
}
