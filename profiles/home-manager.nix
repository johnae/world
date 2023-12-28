{
  hostName,
  inputs,
  ...
}: {
  home-manager.extraSpecialArgs = {inherit hostName inputs;};
  home-manager.sharedModules = [
    ../users/modules/chromiums.nix
    ../users/modules/git-auto-sync.nix
    ../users/modules/kubie.nix
    ../users/modules/river.nix
    ../users/modules/theme.nix
    ../users/modules/userinfo.nix
  ];
}
