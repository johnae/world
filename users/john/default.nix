{ hostName, userProfiles, users, pkgs, lib, config, ... }:

let
  userName = "john";
  userConfig = users.users.${userName};
  groupConfig = users.groups.${userName};
  userSettings = lib.attrByPath [ userName "settings"] {} users;

in

{

  imports = if userSettings ? "desktop" && userSettings.desktop then
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
