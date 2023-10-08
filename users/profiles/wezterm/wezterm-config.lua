local wezterm = require 'wezterm'
local act = wezterm.action
local mux = wezterm.mux
local config = wezterm.config_builder()

local function basename(str)
	local name = string.gsub(str, "(.*/)(.*)", "%2")
	return name
end

local function has_value(t, v)
  for i, value in ipairs(t) do
    if v == value then
      return true
    end
  end
  return false
end

local function open_project_action(window, pane)
  local domain = pane:get_domain_name()
  wezterm.log_info('domain: ', domain)
  local choices = {}
  local seen = {}
  local status, out, err
  if (domain == "remote-dev") then
    status, out, err = wezterm.run_child_process (wezterm.shell_split('ssh sirius fd \\.git /home/john/Development -H -t d -x echo {//}'))
  else
    status, out, err = wezterm.run_child_process (wezterm.shell_split('fd \\.git /home/john/Development -H -t d -x echo {//}'))
  end
  for line in out:gmatch("[^\r\n]+") do
    if (not seen[line]) then
      table.insert(choices, { label = tostring(line) })
      seen[line] = true
    end
  end
  window:perform_action(
    act.InputSelector {
      action = wezterm.action_callback(function(window, pane, id, label)
        if not id and not label then
          wezterm.log_info('cancelled project select')
        else
          local name = basename(label)
          local workspaces = mux.get_workspace_names()
          if not has_value(workspaces, name) then
            wezterm.log_info('create new workspace: ', name)
            local _, pane, _ = mux.spawn_window({
              workspace = name,
              domain = { DomainName = domain },
              cwd = label,
              args = wezterm.shell_split('nu -e "if (\'flake.nix\' | path exists) { nix develop --impure -c hx . } else { hx . }"')
            })
            mux.set_active_workspace(name)
            pane:split { cwd = label, direction = 'Bottom', size = 0.15 }
            pane:activate()
          else
            mux.set_active_workspace(name)
          end
        end
      end),
      title = "Projects",
      choices = choices,
      fuzzy = true,
    },
    pane
  )
end

config.font = wezterm.font 'JetBrainsMono Nerd Font'
config.font_size = 14.0
config.color_scheme = 'nord'
config.hide_tab_bar_if_only_one_tab = true
config.leader = { key='Space', mods='CTRL' }
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
    key = 'Space',
    mods = 'LEADER|CTRL',
    action = act.SendKey { key = 'Space', mods = 'CTRL' },
  },
  {
    key = 'f',
    mods = 'LEADER',
    action = wezterm.action_callback(open_project_action),
  }
}
config.unix_domains = {
  {
    name = "local-dev",
  },
}
config.ssh_domains = {
  {
    name = "remote-dev",
    remote_address = "sirius",
    username = "john",
    connect_automatically = false,
  },
}
return config
