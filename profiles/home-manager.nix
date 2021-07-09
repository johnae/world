{hostName, config, ...}:

let
  inherit (builtins) mapAttrs;
in
{
  home-manager.users = mapAttrs (user: conf:
    { config, ... }:
    {
      imports = [
        ../users/profiles/home.nix
        ../users/profiles/extra-config.nix
        ../users/profiles/theme.nix
      ] ++ conf.profiles;
      home.username = user;
      home.extraConfig.hostName = hostName;
      home.extraConfig.userEmail = conf.email;
      home.extraConfig.userFullName = conf.fullName;
      home.extraConfig.githubUser = conf.githubUser;
      home.extraConfig.gitlabUser = conf.gitlabUser;
    }
  ) config.userConfiguration;
}
