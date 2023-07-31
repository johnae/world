{
  pkgs,
  lib,
  config,
  ...
}: let
  inherit (config) home;
in {
  programs.direnv.enableNushellIntegration = false;
  programs.nushell = {
    enable = true;
    package = pkgs.nushell;
    configFile.source = ./config.nu;
    envFile.source = ./env.nu;
  };
  xdg.configFile."nushell/starship.nu".source = ./starship.nu;
  xdg.configFile."nushell/home.nu".source = pkgs.writeText "home.nu" ''
    ${
      lib.concatStringsSep "\n" (lib.mapAttrsToList (name: value: "$env.${name} = \"${value}\"") home.sessionVariables)
    }
  '';
}
