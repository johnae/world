{
  pkgs,
  config,
  ...
}: let
  inherit (config) userinfo;
in {
  programs.rbw.enable = true;
  programs.rbw.settings = {
    email = userinfo.altEmail;
    lock_timeout = 3600 * 10;
    pinentry = pkgs.pinentry-gnome3;
  };
}
