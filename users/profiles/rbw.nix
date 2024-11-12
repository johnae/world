{
  pkgs,
  config,
  lib,
  ...
}: let
  inherit (config) userinfo;
in {
  programs.rbw.enable = true;
  programs.rbw.settings = {
    inherit (userinfo) email;
    lock_timeout = 3600 * 10;
    pinentry =
      if pkgs.stdenv.isLinux
      then pkgs.pinentry-gnome3
      else pkgs.pinentry_mac;
    base_url = "https://bw.9000.dev";
  };
}
