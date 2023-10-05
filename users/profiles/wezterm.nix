{pkgs, ...}: {
  programs.wezterm = {
    enable = true;
    extraConfig = ''
      local wezterm = require 'wezterm'
      local act = wezterm.action
      local config = wezterm.config_builder()
      config.font = wezterm.font 'JetBrainsMono Nerd Font'
      config.font_size = 14.0
      config.color_scheme = 'nord'
      config.hide_tab_bar_if_only_one_tab = true
      config.leader = { key='a', mods='CTRL' }
      config.window_background_opacity = 0.95
      config.keys = {
        {
          key = 'RightArrow',
          mods = 'LEADER',
          action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' },
        },
        {
          key = 'DownArrow',
          mods = 'LEADER',
          action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' },
        },
        {
          key = 'a',
          mods = 'LEADER|CTRL',
          action = wezterm.action.SendKey { key = 'a', mods = 'CTRL' },
        },

      }
      return config
    '';
  };
}
