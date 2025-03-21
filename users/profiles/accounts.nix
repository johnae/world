{
  mainConfig,
  pkgs,
  tailnet,
  ...
}: {
  accounts.email = {
    maildirBasePath = "Mail";
    accounts."insane" = {
      address = "john@9000.dev";
      userName = "john@9000.dev";
      passwordCommand = "${pkgs.coreutils}/bin/cat ${mainConfig.age.secrets.email-account-pass.path}";
      smtp.host = "icarus.${tailnet}.ts.net";
      smtp.port = 1025;
      realName = "John Axel Eriksson";
      msmtp.enable = true;
      smtp.tls.enable = true;
    };
    accounts."9000" = {
      address = "john@9000.dev";
      userName = "john@9000.dev";
      passwordCommand = "${pkgs.coreutils}/bin/cat ${mainConfig.age.secrets.email-account-pass.path}";
      smtp.host = "icarus.${tailnet}.ts.net";
      smtp.port = 1025;
      imap.host = "icarus.${tailnet}.ts.net";
      imap.port = 1143;
      mbsync = {
        enable = true;
        create = "maildir";
        expunge = "none";
        subFolders = "Verbatim";
        patterns = ["All Mail"];
      };
      realName = "John Axel Eriksson";
      msmtp.enable = true;
      notmuch.enable = true;
      primary = true;
      smtp.tls.enable = true;
    };
  };
}
