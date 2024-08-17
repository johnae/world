{
  pkgs,
  lib,
  config,
  ...
}: let
  ## {
  ##   user1 = ["keyHandle,publicKey,es256,+presence" "keyHandle,publicKey,es256,+presence"];
  ## }
  ##
  inherit (config) u2fMappings;
  writeU2fKeys = userConfigs:
    pkgs.writeText "u2f_keys" (lib.concatStringsSep "\n" (
      lib.mapAttrsToList (
        user: configs: "${user}:${lib.concatStringsSep ":" configs}"
      )
      userConfigs
    ));
in {
  security.pam.u2f.settings.cue = true;
  security.pam.u2f.settings.origin = "pam://1337";
  security.pam.u2f.settings.appid = "pam://9000";
  security.pam.u2f.settings.enable = true;
  security.pam.u2f.settings.control = "required";
  security.pam.u2f.settings.authfile = writeU2fKeys u2fMappings;

  assertions = [
    {
      assertion = builtins.length (builtins.attrNames u2fMappings) > 0;
      message = "Only enabling pamu2f if u2fmappings is not empty";
    }
  ];
}
