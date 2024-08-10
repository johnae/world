-- wezterm is automatically imported by home-manager
local act = wezterm.action
local mux = wezterm.mux
local config = wezterm.config_builder()

local function starts_with(str, start)
  return str:sub(1, #start) == start
end

local function project_name(str)
  if not starts_with(str, '/') then
    return str
  end
  local name = string.gsub(str, "(.*/)(.*)", "%2")
  local dirname_path = string.gsub(str, "(.*)/(.*)", "%1")
  local dirname = string.gsub(dirname_path, "(.*/)(.*)", "%2")
  if dirname ~= 'Development' then
    name = dirname .. "/" ..  name
  end
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

local function find_tab(t, v)
  for _, tab in ipairs(t) do
    if tab:get_title() == v then
      return tab
    end
  end
  return nil
end

local function run_child_process(window, pane, args)
  local domain = pane:get_domain_name()
  for _,ssh_domain in pairs(config.ssh_domains) do
    if ssh_domain.name == domain then
      args = { 'ssh', ssh_domain.username .. '@' .. ssh_domain.remote_address, unpack(args) }
      break
    end
  end
  wezterm.log_info('run cmd: ', args)
  status, out, err = wezterm.run_child_process(args)
  wezterm.log_info('result - status: ', status, ' out: ', out, ' err: ', err)
  return status, out, err
end

local function spawn_project_window(window, pane)
  local domain = pane:get_domain_name()
  local cwd = pane:get_current_working_dir()
  mux.spawn_window { domain = { DomainName = domain }, cwd = cwd.file_path }
end

local function open_project_action(window, pane)
  local domain = pane:get_domain_name()
  local cwd = pane:get_current_working_dir()
  local fd_cmd = 'fd \\.git$ /home/john/Development -d 3 -H -t d -x echo {//}'
  status, out, err = run_child_process(window, pane, wezterm.shell_split(fd_cmd))
  local choices = {}
  local seen = {}
  local workspaces = mux.get_workspace_names()
  local wstable = {}
  for _, ws in ipairs(workspaces) do
    table.insert(choices, { id = ws, label = ws })
    wstable[ws] = true
    seen[ws] = true
  end
  for line in out:gmatch("[^\r\n]+") do
    local name = project_name(line)
    if (not seen[name]) then
      table.insert(choices, { id = tostring(line), label = name })
      seen[name] = true
    end
  end

  window:perform_action(
    act.InputSelector {
      action = wezterm.action_callback(function(window, pane, id, label)
        if not id and not label then
          wezterm.log_info('cancelled project select')
        else
          wezterm.log_info('select input, id: ', id, ' label: ', label)
          local name = project_name(id)
          if not wstable[name] then
            wezterm.log_info('spawn window in ws: ', name)
            mux.spawn_window { domain = { DomainName = domain }, workspace = name, cwd = id,
              args = wezterm.shell_split('nu -e "cd ' .. id .. '; if (\'.envrc\' | path exists) { direnv exec . hx . } else { hx . }"')
            }
          end
          wezterm.log_info('set active ws: ', name)
          mux.set_active_workspace(name)
        end
      end),
      title = "Projects",
      choices = choices,
      fuzzy = true,
    },
    pane
  )
end


-- The filled in variant of the < symbol
local SOLID_LEFT_ARROW = wezterm.nerdfonts.pl_right_hard_divider

-- The filled in variant of the > symbol
local SOLID_RIGHT_ARROW = wezterm.nerdfonts.pl_left_hard_divider

local REMOTE_DEV_COLOR = '#1e1e2e'
local LOCAL_DEV_COLOR = '#ae4d1a'
local LOCAL_TERM_COLOR = '#22336e'

config.use_fancy_tab_bar = false
config.tab_max_width = 64
config.show_tabs_in_tab_bar = false
config.show_new_tab_button_in_tab_bar = false

wezterm.on('update-status', function(window, pane)

  local tab_bg = LOCAL_TERM_COLOR
  local domain = pane:get_domain_name()
  local overrides = window:get_config_overrides() or {}
  if domain == 'remote-dev' then
    tab_bg = REMOTE_DEV_COLOR
  elseif domain == 'local-dev' then
    tab_bg = LOCAL_DEV_COLOR
  end
  overrides.colors = {
    tab_bar = {
      background = tab_bg
    }
  }
  window:set_config_overrides(overrides)
  
  -- Each element holds the text for a cell in a "powerline" style << fade
  local cells = {}

  -- Figure out the cwd and host of the current pane.
  -- This will pick up the hostname for the remote host if your
  -- shell is using OSC 7 on the remote host.
  local cwd_uri = pane:get_current_working_dir()
  local domain = pane:get_domain_name()

  local ws = window:active_workspace()
  if cwd_uri then
    local cwd = ''
    local hostname = ''

    if type(cwd_uri) == 'userdata' then
      -- Running on a newer version of wezterm and we have
      -- a URL object here, making this simple!

      cwd = cwd_uri.file_path
      hostname = cwd_uri.host or wezterm.hostname()
    else
      -- an older version of wezterm, 20230712-072601-f4abf8fd or earlier,
      -- which doesn't have the Url object
      cwd_uri = cwd_uri:sub(8)
      local slash = cwd_uri:find '/'
      if slash then
        hostname = cwd_uri:sub(1, slash - 1)
        -- and extract the cwd from the uri, decoding %-encoding
        cwd = cwd_uri:sub(slash):gsub('%%(%x%x)', function(hex)
          return string.char(tonumber(hex, 16))
        end)
      end
    end

    -- Remove the domain name portion of the hostname
    local dot = hostname:find '[.]'
    if dot then
      hostname = hostname:sub(1, dot - 1)
    end
    if hostname == '' then
      hostname = wezterm.hostname()
    end

    table.insert(cells, "")
    table.insert(cells, domain .. '/' .. hostname)
    table.insert(cells, ws)
  end

  -- The powerline < symbol
  local LEFT_ARROW = utf8.char(0xe0b3)
  -- The filled in variant of the < symbol
  local SOLID_LEFT_ARROW = utf8.char(0xe0b2)

  -- Color palette for the backgrounds of each cell
  local colors = {
    tab_bg,
    '#51576d',
    '#838ba7',
    '#7287fd',
    '#04a5e5',
    '#04a5e5',
    '#7287fd',
  }

  -- Foreground color for the text across the fade
  local text_fg = '#c0c0c0'

  -- The elements to be formatted
  local elements = {}
  -- How many cells have been formatted
  local num_cells = 0

  -- Translate a cell into elements
  function push(text, is_last)
    local cell_no = num_cells + 1
    table.insert(elements, { Foreground = { Color = text_fg } })
    table.insert(elements, { Background = { Color = colors[cell_no] } })
    table.insert(elements, { Text = ' ' .. text .. ' ' })
    if not is_last then
      table.insert(elements, { Foreground = { Color = colors[cell_no + 1] } })
      table.insert(elements, { Text = SOLID_LEFT_ARROW })
    end
    num_cells = num_cells + 1
  end

  while #cells > 0 do
    local cell = table.remove(cells, 1)
    push(cell, #cells == 0)
  end

  window:set_right_status(wezterm.format(elements))
end)


local skip_hack = false
local function open_gex_action(window, pane)
  skip_hack = true
  local domain = pane:get_domain_name()
  local cwd = pane:get_current_working_dir()
  wezterm.mux.spawn_window { cwd = cwd.file_path, domain = { DomainName = domain }, args = { "gex" } }
  status, out, err = wezterm.run_child_process (wezterm.shell_split('bash -c "sleep 0.1; riverctl toggle-float"'))
end

wezterm.on('FindProject', open_project_action)
wezterm.on('NewProjectWindow', spawn_project_window)

-- hack for wayland / river as wezterm doesn't play very nice here
local current_ws = "none"
local current_num_windows = 0
wezterm.on('window-focus-changed', function(window, pane)
  local domain = pane:get_domain_name()
  local overrides = window:get_config_overrides() or {}
  if domain == 'local' then
    overrides.colors = {
      background = '#00374e'
    }
  elseif domain == 'local-dev' then
    overrides.colors = {
      background = '#0f291f'
    }
  end
  window:set_config_overrides(overrides)
  
  if skip_hack then
    skip_hack = false
    return
  end
-- hack for wayland / river as wezterm doesn't play very nice here
  ws = window:active_workspace()
  local gui_windows = wezterm.gui.gui_windows()
  num_windows = #gui_windows
  if ws ~= current_ws or num_windows ~= current_num_windows then
    wezterm.log_info('hacky-fix')
    status, out, err = wezterm.run_child_process (wezterm.shell_split('riverctl toggle-fullscreen'))
    wezterm.time.call_after(0.05, function()
      status, out, err = wezterm.run_child_process (wezterm.shell_split('riverctl toggle-fullscreen'))
    end)
  end
  current_ws = ws
  current_num_windows = num_windows
end)

wezterm.on('ActivateContextUI', open_gex_action)

-- hack for failing direnv loading
wezterm.on('ReloadFixup', function(window, pane)
  run_child_process(window, pane, wezterm.shell_split('pkill -HUP direnv'))
end)

config.mux_env_remove = {}
config.adjust_window_size_when_changing_font_size = false
config.enable_wayland = true
config.enable_tab_bar = true
config.tab_bar_at_bottom = true
config.hide_tab_bar_if_only_one_tab = false
config.font = wezterm.font 'JetBrainsMono Nerd Font'
config.font_size = 14.0
config.color_scheme = 'Catppuccin Frappé (Gogh)'
config.colors = {
  -- Use a different background color than the theme background color
  -- background = '#2e3440',
}
-- local dimmer = { brightness = 0.2 }
-- config.background = {
--   {
--     source = {
--       File = '/home/john/Sync/wez.png',
--     },
--     hsb = dimmer,
--   },
-- }
config.leader = { key='Space', mods='CTRL' }
config.window_background_opacity = 0.91
config.keys = {
  {
    key = 'Space',
    mods = 'LEADER|CTRL',
    action = act.SendKey { key = 'Space', mods = 'CTRL' },
  },
  {
    key = 'q',
    mods = 'LEADER',
    action = wezterm.action.CloseCurrentPane { confirm = true },
  },
  {
    key = 'c',
    mods = 'LEADER',
    action = act.EmitEvent('ActivateContextUI')
  },
  {
    key = 'n',
    mods = 'LEADER',
    action = act.EmitEvent('NewProjectWindow')
  },
  {
    key = 'q',
    mods = 'LEADER|SHIFT',
    action = wezterm.action.CloseCurrentTab { confirm = true },
  },
  {
    key = 'r',
    mods = 'LEADER',
    action = act.EmitEvent('ReloadFixup')
  },
  {
    key = 'g',
    mods = 'LEADER',
    action = wezterm.action.ShowTabNavigator
  },
  {
    key = 'a',
    mods = 'LEADER',
    action = act.EmitEvent('FindProject')
  },
  {
    key = 'f',
    mods = 'LEADER',
    action = wezterm.action.TogglePaneZoomState,
  },
  {
    key = 'w',
    mods = 'LEADER',
    action = act.ShowLauncherArgs {
      flags = 'FUZZY|WORKSPACES'
    },
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
    remote_address = "orion",
    username = "john",
  },
}
return config

