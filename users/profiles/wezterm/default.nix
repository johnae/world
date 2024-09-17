{
  config,
  pkgs,
  ...
}: let
  inherit (config) xdg home;
in {
  xdg.configFile."wezterm/wezterm.fnl.lua".source =
    pkgs.runCommand "wezterm.fnl.lua" {
      nativeBuildInputs = [pkgs.fennel];
    }
    ''
      fennel --compile ${./wezterm.fnl} > $out
    '';
  programs.wezterm = {
    enable = true;
    extraConfig = ''
      _G.wezterm = wezterm
      _G.dev_remote = "${config.userinfo.devRemote}"
      local config
      if os.getenv("WEZTERM_FNL") then
        -- fake
        local function getinfo()
        end
        _G.debug = {getinfo = getinfo}
        local fennel = dofile("${pkgs.lua54Packages.fennel}/share/lua/5.4/fennel.lua")
        config = fennel.dofile(os.getenv("WEZTERM_FNL"))
      else
        config = dofile("${home.homeDirectory}/${xdg.configFile."wezterm/wezterm.fnl.lua".target}")
      end
      return config
    '';
  };
}
