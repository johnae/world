{
  xdg.configFile."wezterm/wezterm-config.lua".source = ./wezterm-config.lua;
  programs.wezterm = {
    enable = true;
    extraConfig = ''
      return (require 'wezterm-config')
    '';
  };
}
