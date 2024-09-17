{
  config,
  pkgs,
  ...
}: let
  inherit (config) xdg;
in {
  xdg.configFile."river-luatile/layout.fnl.lua".source =
    pkgs.runCommand "layout.fnl.lua" {
      nativeBuildInputs = [pkgs.fennel];
    }
    ''
      fennel --compile ${./layout.fnl} > $out
    '';
  xdg.configFile."river-luatile/layout.lua".source = pkgs.writeText "layout.lua" ''
    if os.getenv("LUATILE_FNL") then
      -- fake
      local function getinfo()
      end
      _G.debug = {getinfo = getinfo}
      local fennel = dofile("${pkgs.lua54Packages.fennel}/share/lua/5.4/fennel.lua")
      fennel.dofile(os.getenv("LUATILE_FNL"))
    else
      dofile("${xdg.configFile."river-luatile/layout.fnl.lua".target}")
    end
  '';
}
