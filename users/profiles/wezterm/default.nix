{
  config,
  pkgs,
  ...
}: let
  inherit (config) xdg home;
in {
  xdg.configFile."wezterm/workspace.yaml".source = pkgs.writeText "workspace.yaml" ''
    windows:
    - command: direnv exec . hx .
      name: editor
      restart: true
      panes:
      - size: 0.30
        name: aichat
        command: direnv exec . claude
        direction: Right
      - size: 0.25
        name: term
        split_from: 1
        direction: Bottom
        command: nu
  '';
  xdg.configFile."wezterm/wezterm.fnl.lua".source =
    pkgs.runCommand "wezterm.fnl.lua" {
      nativeBuildInputs = [pkgs.fennel];
    }
    ''
      fennel --compile ${./wezterm.fnl} > $out
    '';
  xdg.configFile."wezterm/wezterm.lua".source = pkgs.writeText "wezterm.lua" ''
    local wezterm = require 'wezterm'
    _G.wezterm = wezterm
    _G.dev_remote = "${config.userinfo.devRemote}"
    -- fake
    local function getinfo()
    end
    local function traceback()
    end
    _G.debug = {getinfo = getinfo, traceback = traceback}
    local config
    if os.getenv("WEZTERM_FNL") then
      local fennel = dofile("${pkgs.lua54Packages.fennel}/share/lua/5.4/fennel.lua")
      config = fennel.dofile(os.getenv("WEZTERM_FNL"))
    else
      config = dofile("${home.homeDirectory}/${xdg.configFile."wezterm/wezterm.fnl.lua".target}")
    end
    ${
      if pkgs.stdenv.isDarwin
      then ''
        config.front_end = "WebGpu"
        config.macos_window_background_blur = 20
      ''
      else ""
    }
    return config
  '';
  programs.wezterm = {
    enable = true;
    # extraConfig = ''
    # '';
  };
}
