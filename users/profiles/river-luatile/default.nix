{
  config,
  pkgs,
  ...
}: let
  inherit (config.xdg) configHome;
in {
  xdg.configFile."river-luatile/layout.fnl".source = ./layout.fnl;
  xdg.configFile."river-luatile/layout.lua".source = pkgs.writeText "layout.lua" ''
    -- fake
    local function getinfo()
    end
    _G.debug = {getinfo = getinfo}
    local fennel = dofile("${pkgs.lua54Packages.fennel}/share/lua/5.4/fennel.lua")
    fennel.dofile("${configHome}/river-luatile/layout.fnl")
  '';
}
