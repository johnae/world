{pkgs, ...}: {
  services.mako.settings = {
    enable = true;
    font = "Roboto";
    background-color = "#000021DD";
    text-color = "#FFFFFFFF";
    border-size = 0;
    border-radius = 15;
    icons = true;
    icon-path = "${pkgs.moka-icon-theme}/share/icons/Moka";
    markup = true;
    actions = true;
    default-timeout = 3000;
    padding = "20";
    height = 200;
    width = 500;
    layer = "overlay";
  };
}
