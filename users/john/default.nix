{ hostName, userProfiles, hostConfig, pkgs, lib, config, ... }:

let
  userName = "john";
  userConfig = hostConfig.users.users.${userName};
  groupConfig = hostConfig.users.groups.${userName};
  userSettings = lib.attrByPath [ userName ] {} hostConfig.users;

in

{

  imports = if userSettings ? "profiles" then
    [ ./desktop.nix ]
  else [ ];

  nix.trustedUsers = [ userName ];

  users.groups = {
    ${userName} = groupConfig;
  };

  users.users."${userName}" = ({
    shell = pkgs.fish;
    extraGroups = [ hostName "wheel" "docker" "video" "audio" "plugdev" ];
    isNormalUser = true;
  } // userConfig);

}
