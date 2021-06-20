{config, ...}:

let
  extraConfig = config.home.extraConfig;
in
{
  programs.rbw.enable = true;
  programs.rbw.settings = {
    email = extraConfig.userEmail;
    lock_timeout = 3600;
    pinentry = "gnome3";
  };
}
