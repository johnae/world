{ pkgs, config, lib, options }:

{
  programs.mbsync = {
    enable = true;
  };
}
