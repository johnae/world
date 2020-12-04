{ pkgs, config, lib, options, ... }:
let
  flavor = "gmail.com";
  realName = "John Axel Eriksson";

  lieer = {
    enable = true;
    ignoreTagsLocal = [ "new" ];
    sync.enable = true;
  };

  notmuch = { enable = true; };

in
{
  accounts.email = {
    maildirBasePath = ".mail";
    accounts = {
      personal = {
        primary = true;
        address = "john@insane.se";
        maildir.path = "personal";
        inherit realName flavor lieer notmuch;
      };
      professional = {
        address = "john@instabox.se";
        aliases = [ "john@instabox.io" ];
        maildir.path = "professional";
        inherit realName flavor lieer notmuch;
      };
    };
  };
}
