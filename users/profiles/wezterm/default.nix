{
  xdg.configFile."wezterm/wezterm-config.lua".source = ./wezterm-config.lua;
  programs.wezterm = {
    enable = true;
    extraConfig = ''
      package.path = "./?.lua;" .. package.path
      return (require 'wezterm-config')
    '';
  };
}
