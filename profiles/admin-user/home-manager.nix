{
  adminUser,
  config,
  lib,
  pkgs,
  ...
}: {
  # Grant admin user read access to SSH host key for HM agenix decryption
  systemd.tmpfiles.rules =
    [
      "a+ /etc/ssh/ssh_host_ed25519_key - - - - u:${adminUser.name}:r"
    ]
    ++ lib.optionals config.ephemeralRoot [
      "a+ /keep/etc/ssh/ssh_host_ed25519_key - - - - u:${adminUser.name}:r"
    ];

  home-manager = {
    users.${adminUser.name} = {
      home.username = "${adminUser.name}";
      inherit (adminUser) userinfo;
      programs = {
        git = {
          signing.format = "ssh";
          settings = {
            commit.gpgSign = true;
            tag.forceSignAnnotated = true;
          };
          includes = [
            {
              condition = "gitdir:~/Development/volvo/"; ## note the IMPORTANT trailing slash
              contents.user.email = "john.axel.eriksson@consultant.volvo.com";
              contents.commit.gpgSign = false;
              contents.credential."https://github.com".helper = "${pkgs.scripts}/bin/rbw-git-creds github.com john.axel.eriksson@consultant.volvo.com";
              contents.url."https://github.com".insteadOf = "ssh://git@github.com";
            }
          ];
        };
      };
    };
  };
}
