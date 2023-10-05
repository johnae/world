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
    action = act.SplitHorizontal { domain = 'CurrentPaneDomain' },
  },
  {
    key = 'DownArrow',
    mods = 'LEADER',
    action = act.SplitVertical { domain = 'CurrentPaneDomain' },
  },
  {
    key = 'a',
    mods = 'LEADER|CTRL',
    action = act.SendKey { key = 'a', mods = 'CTRL' },
  },
}
-- config.unix_domains = {
--   {
--     name = "dev",
--   },
-- }
-- config.ssh_domains = {
--   {
--     name = "sirius",
--     remote_address = "sirius",
--     username = "john",
--     connect_automatically = true,
--   },
-- }

return config
