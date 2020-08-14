{ pkgs, config, lib, options }:
let
  flavor = "gmail.com";
  realName = "John Axel Eriksson";

  lieer = {
    enable = true;
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
        maildir.path = "professional";
        inherit realName flavor lieer notmuch;
      };
      work = {
        address = "john@karma.life";
        maildir.path = "work";
        inherit realName flavor lieer notmuch;
      };
    };
  };
}
