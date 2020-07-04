{ pkgs, config, lib, options }:
let
  flavor = "gmail.com";
  realName = "John Axel Eriksson";

  imapnotify = {
    enable = true;
    boxes = [ "Inbox" ];
    onNotify = "${pkgs.isync}/in/mbsync -a";
    onNotifyPost = {
      mail = "${pkgs.emacs-run}/bin/emacs-run -e '(mu4e-update-mail-and-index t)'";
    };
  };
  mbsync = {
    enable = true;
    create = "both";
    expunge = "both";
    patterns = [
      "*"
      "![Gmail]*"
      "[Gmail]/Sent Mail"
      "[Gmail]/All Mail"
      "[Gmail]/Trash"
    ];
    extraConfig = {
      account = {
        PipelineDepth = 50;
        Timeout = 60;
      };
      channel = {
        SyncState = "*";
        CopyArrivalDate = "yes";
      };
    };
  };

  genGmailPasswordCommand = address: "${pkgs.gnupg}/bin/gpg2 -q --for-your-eyes-only --no-tty -d " + "${config.programs.password-store.settings.PASSWORD_STORE_DIR}/emacs/auth/authinfo.gpg | " + "${pkgs.gawk}/bin/awk '/machine imap.gmail.com login ${address}/ {print $NF}'";
in
{
  accounts.email = {
    maildirBasePath = ".mail";
    accounts = {
      personal = rec {
        primary = true;
        address = "john@insane.se";
        maildir.path = "personal";
        passwordCommand = genGmailPasswordCommand address;
        inherit imapnotify flavor mbsync realName;
      };
      professional = rec {
        address = "john@instabox.se";
        maildir.path = "professional";
        passwordCommand = genGmailPasswordCommand address;
        inherit imapnotify flavor mbsync realName;
      };
      work = rec {
        address = "john@karma.life";
        maildir.path = "work";
        passwordCommand = genGmailPasswordCommand address;
        inherit imapnotify flavor mbsync realName;
      };
    };
  };
}
