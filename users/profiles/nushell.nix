{pkgs, ...}: {
  programs.nushell = {
    enable = true;
    package = pkgs.nu;
    configFile.source = ./nushell/config.nu;
    envFile.source = ./nushell/env.nu;
  };
  xdg.configFile."nushell/starship.nu".source = ./nushell/starship.nu;
}
