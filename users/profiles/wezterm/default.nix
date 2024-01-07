{
  pkgs,
  config,
  ...
}: {
  xdg.configFile."wezterm/wezterm-config.lua".source = pkgs.runCommand "wezterm-config.lua" {} ''
    cp ${./wezterm-config.lua} $out
    substituteInPlace $out --replace "<devRemote>" ${config.userinfo.devRemote}
  '';
  programs.wezterm = {
    enable = true;
    extraConfig = ''
      return (require 'wezterm-config')
    '';
  };
}
