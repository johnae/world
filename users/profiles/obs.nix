{ pkgs, config, lib, options, ... }:
{
  programs.obs-studio = {
    enable = true;
    plugins = [ pkgs.obs-wlrobs ];
  };
}
