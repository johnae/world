{config, ...}:

let
  userinfo = config.userinfo;
in
{
  programs.rbw.enable = true;
  programs.rbw.settings = {
    email = userinfo.email;
    lock_timeout = 3600;
    pinentry = "gnome3";
  };
}
