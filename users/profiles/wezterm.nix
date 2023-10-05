{pkgs, ...}: {
  programs.wezterm = {
    enable = true;
    extraConfig = ''
      local wezterm = require 'wezterm'
      local config = wezterm.config_builder()
      config.font = wezterm.font 'JetBrainsMono Nerd Font'
      config.font_size = 14.0
      config.color_scheme = 'Nord'
      config.hide_tab_bar_if_only_one_tab = true
      return config
    '';
  };
}
