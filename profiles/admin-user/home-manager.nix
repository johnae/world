{
  adminUser,
  pkgs,
  ...
}: {
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
