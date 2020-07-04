{ pkgs, config, lib, options }:

rec {
  programs.mako = {
    enable = true;
    font = "Roboto";
    backgroundColor = "#000021DD";
    textColor = "#FFFFFFFF";
    borderSize = 0;
    borderRadius = 15;
    icons = true;
    iconPath = "${pkgs.moka-icon-theme}/share/icons/Moka";
    markup = true;
    actions = true;
    defaultTimeout = 3000;
    padding = "20";
    height = 200;
    width = 500;
    layer = "overlay";
  };
}
