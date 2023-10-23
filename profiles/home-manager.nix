{
  hostName,
  inputs,
  userProfiles,
  ...
}: let
  inherit (builtins) mapAttrs;
in {
  home-manager.extraSpecialArgs = {inherit hostName inputs;};
  home-manager.sharedModules = [
    ../users/modules/git-auto-sync.nix
    ../users/modules/kubie.nix
    ../users/modules/chromiums.nix
    ../users/modules/theme.nix
    ../users/modules/userinfo.nix
  ];

  home-manager.users =
    mapAttrs (
      user: profiles: {...}: {
        imports = [../users/profiles/home.nix] ++ profiles;
        home.stateVersion = "21.05";
        home.username = user;
      }
    )
    userProfiles;
}
