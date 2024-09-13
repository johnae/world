{
  config,
  pkgs,
  ...
}: {
  programs.wezterm = {
    enable = true;
    extraConfig = ''
      -- fake
      local function getinfo()
      end
      _G.debug = {getinfo = getinfo}
      local fennel = dofile("${pkgs.lua54Packages.fennel}/share/lua/5.4/fennel.lua")
      wezterm.log_error("loaded fennel")
      _G["wezterm"] = wezterm
      _G["dev_remote"] = "${config.userinfo.devRemote}"
      local config = fennel.dofile("${./wezterm.fnl}")
      wezterm.log_error("evaluated wezterm.fnl")
      return config
    '';
  };
}
