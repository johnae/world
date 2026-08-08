{
  hostName,
  inputs,
  config,
  tailnet,
  adminUser,
  pkgs,
  ...
}: {
  home-manager.extraSpecialArgs = {inherit hostName inputs tailnet adminUser;};

  # Without this, activation aborts if it would clobber an existing unmanaged
  # path - which takes down the whole user session (bar/shell never start) the
  # first time a generation changes how a path is linked (dir -> symlink, real
  # file -> symlink, etc). Back the offending path up and carry on instead.
  home-manager.backupFileExtension = "hm-bak";

  home-manager.sharedModules =
    [
      inputs.agenix.homeManagerModules.age
      inputs.agenix-rekey.homeManagerModules.default
      {
        age.identityPaths = [
          (
            if config.ephemeralRoot
            then "/keep/etc/ssh/ssh_host_ed25519_key"
            else "/etc/ssh/ssh_host_ed25519_key"
          )
        ];
        age.rekey = {
          inherit (config.age.rekey) hostPubkey masterIdentities;
          storageMode = "local";
          localStorageDir = config.age.rekey.localStorageDir + "-hm";
        };
      }
      ../users/modules/default.nix
    ]
    ++ (
      if pkgs.stdenv.isDarwin
      then [inputs.mac-app-util.homeManagerModules.default]
      else [
        inputs.niri.homeModules.niri
        inputs.noctalia.homeModules.default
        inputs.zen-browser.homeModules.default
      ]
    );
}
