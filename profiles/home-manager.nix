{
  hostName,
  inputs,
  config,
  tailnet,
  adminUser,
  pkgs,
  ...
}: {
  home-manager.extraSpecialArgs = {inherit hostName inputs tailnet adminUser;} // {mainConfig = config;};

  home-manager.sharedModules =
    [
      ../users/modules/chromiums.nix
      ../users/modules/git-auto-sync.nix
      ../users/modules/kubie.nix
      ../users/modules/theme.nix
      ../users/modules/userinfo.nix
    ]
    ++ (
      if pkgs.stdenv.isDarwin
      then [inputs.mac-app-util.homeManagerModules.default]
      else []
    );
}
