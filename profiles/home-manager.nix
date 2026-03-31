{
  hostName,
  inputs,
  config,
  tailnet,
  adminUser,
  pkgs,
  ...
}: {
  # Grant admin user read access to SSH host key for HM agenix decryption
  systemd.tmpfiles.rules = [
    "a+ /etc/ssh/ssh_host_ed25519_key - - - - u:${adminUser.name}:r"
  ];

  home-manager.extraSpecialArgs = {inherit hostName inputs tailnet adminUser;};

  home-manager.sharedModules =
    [
      inputs.agenix.homeManagerModules.age
      inputs.agenix-rekey.homeManagerModules.default
      {
        age.identityPaths = ["/etc/ssh/ssh_host_ed25519_key"];
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
