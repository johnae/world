{
  pkgs,
  lib,
  config,
  ...
}: let
  inherit (config) home;
in {
  programs.atuin.enable = true;
  programs.direnv.enableNushellIntegration = false;
  programs.nushell = {
    enable = true;
    package = pkgs.nushell;
    configFile.source = ./config.nu;
    envFile.source = ./env.nu;
    extraConfig = ''
      source ~/.config/nushell/home.nu
      source ~/.config/nushell/starship.nu
      source ~/.config/nushell/atuin.nu
    '';
  };
  xdg.configFile."nushell/atuin.nu".source = pkgs.runCommand "atuin.nu" {} ''
    ${pkgs.atuin}/bin/atuin init nu > $out
  '';
  xdg.configFile."nushell/starship.nu".source = ./starship.nu;
  xdg.configFile."nushell/home.nu".source = pkgs.writeText "home.nu" ''
    ${
      lib.concatStringsSep "\n" (lib.mapAttrsToList (name: value: "$env.${name} = \"${value}\"") home.sessionVariables)
    }
  '';
}
