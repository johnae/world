{
  hostName,
  adminUser,
  ...
}: {
  age.rekey = {
    hostPubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFxKBZHPNEUvAkyglnezNarJdSDI/J8TivNsmyGTb0xs";
    storageMode = "local";
    localStorageDir = ../../../secrets/rekeyed + "/${hostName}";
  };

  imports = [
    ../../../profiles/defaults.nix
    ../../../profiles/kubevirt.nix
    ../../../profiles/tailscale.nix
    ../../../profiles/zram.nix
    ../../../profiles/admin-user/user.nix
    ../../../profiles/home-manager.nix
    ../../../profiles/admin-user/home-manager.nix
  ];

  system.autoUpgrade = {
    enable = true;
    flake = "github:johnae/world";
    allowReboot = true;
    dates = "06:00";
    randomizedDelaySec = "5min";
  };

  services.tailscale.auth = {
    enable = true;
    args.ssh = true;
    args.accept-routes = false;
    args.accept-dns = true;
    args.auth-key = "file:/mnt/kubevirt-ts-auth/ts-auth-key";
    args.hostname = hostName;
    args.advertise-tags = ["tag:server"];
  };

  programs.ssh.startAgent = true;

  age.secrets.id_ed25519_alt = {
    rekeyFile = ../../../secrets/id_ed25519_alt.age;
    owner = "${toString adminUser.uid}";
    path = "/home/${adminUser.name}/.ssh/id_ed25519_alt";
  };

  home-manager.users.${adminUser.name} = {
    imports = [
      ../../../users/profiles/headless.nix
      ../../../users/profiles/9k.nix
    ];
  };

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIzm5RyD+1nfy1LquvkEog4SZtPgdhzjr49jSC8PAinp"
  ];
}
