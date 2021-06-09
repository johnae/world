{ hostName, userProfiles, users, pkgs, lib, ... }:

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
    scard.gid = 1050;
  };

  users.users."${userName}" = ({
    shell = pkgs.fish;
    extraGroups = [ "scard" hostName ];
    isNormalUser = true;
  } // userConfig);

}
