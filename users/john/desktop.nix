{ hostName, userProfiles, users, pkgs, lib, ... }:

let
  userName = "john";
  userConfig = users.users.${userName};
  groupConfig = users.groups.${userName};
  userSettings = lib.attrByPath [ userName ] {} users;
in
{
  home-manager.users.${userName} = { config, ... }:
  let
    extraConfig = config.home.extraConfig;
  in

    {
    imports = [
      userProfiles.home
      userProfiles.extra-config
      userProfiles.theme
    ] ++ map (profile: userProfiles.${profile}) (lib.attrByPath [ "profiles"] [] userSettings);

    home.username = userName;
    home.extraConfig.hostName = hostName;
    home.extraConfig.userEmail = userSettings.email;
    home.extraConfig.userFullName = userSettings.fullName;
    home.extraConfig.githubUser = userSettings.githubUser;
    home.extraConfig.gitlabUser = userSettings.gitlabUser;
  };

  environment.state."/keep" =
    {
      users.${toString userConfig.uid} = {
        directories = [
          "/home/${userName}/Downloads"
          "/home/${userName}/Documents"
          "/home/${userName}/Development"
          "/home/${userName}/Photos"
          "/home/${userName}/Pictures"
          "/home/${userName}/Games"
          "/home/${userName}/Sync"
          "/home/${userName}/.local/share/direnv"
          "/home/${userName}/.local/share/password-store"
          "/home/${userName}/.local/share/fish"
          "/home/${userName}/.local/share/containers"
          "/home/${userName}/.local/share/Steam"
          "/home/${userName}/.local/share/vulkan"
          "/home/${userName}/.mail"
          "/home/${userName}/.cargo"
          "/home/${userName}/.cache/mu"
          "/home/${userName}/.cache/nix"
          "/home/${userName}/.cache/nix-index"
          "/home/${userName}/.mozilla/firefox/default"
          "/home/${userName}/.gnupg"
          "/home/${userName}/.config/gcloud"
          "/home/${userName}/.emacs.d"
        ];
        files = [
          "/home/${userName}/.kube/config"
          "/home/${userName}/.ssh/known_hosts"
          "/home/${userName}/.spotify_token_cache.json"
        ];
      };
    };

}
