{config, ...}: let
  inherit (config) userinfo;
in {
  programs.rbw.enable = true;
  programs.rbw.settings = {
    inherit (userinfo) email;
    lock_timeout = 3600;
    pinentry = "gnome";
    base_url = "https://bw.9000.dev";
  };
}
