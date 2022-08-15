{pkgs, ...}: {
  programs.nushell = {
    enable = true;
    package = pkgs.nu;
    configFile.source = ./config.nu;
    envFile.source = ./env.nu;
  };
  xdg.configFile."nushell/starship.nu".source = ./starship.nu;
}
