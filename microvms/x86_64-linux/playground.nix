{
  adminUser,
  config,
  hostName,
  ...
}: {
  publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPRUfwPUxHqtVRsV3CdDRDEAYTg28ZdK5/Mz/GlcZdiv";
  imports = [
    ../../profiles/admin-user/home-manager.nix
    ../../profiles/admin-user/user.nix
    ../../profiles/home-manager.nix
    ../../profiles/microvm.nix
    ../../profiles/tailscale.nix
    ../../profiles/zram.nix
  ];

  home-manager.useUserPackages = true;
  home-manager.useGlobalPkgs = true;

  age.secrets = {
    ts-google-9k = {
      file = ../../secrets/ts-google-9k.age;
    };
  };

  age.identityPaths = ["/microvm-secrets/ssh_host_ed25519_key"];

  services.openssh.hostKeys = [
    {
      path = "/microvm-secrets/ssh_host_ed25519_key";
      type = "ed25519";
    }
  ];

  services.tailscale.auth = {
    enable = true;
    args.advertise-tags = ["tag:server"];
    args.ssh = true;
    args.accept-routes = false;
    args.accept-dns = true;
    args.advertise-exit-node = true;
    args.auth-key = "file:/var/run/agenix/ts-google-9k";
  };

  fileSystems."/microvm-secrets".neededForBoot = true;
  fileSystems."/keep".neededForBoot = true;

  microvm.writableStoreOverlay = "/nix/.rw-store";
  microvm.volumes = [
    {
      image = "nix-store-overlay.img";
      mountPoint = config.microvm.writableStoreOverlay;
      size = 2048;
    }
  ];
  microvm.shares = [
    {
      proto = "virtiofs";
      tag = "microvm-secrets";
      source = "/var/lib/microvm-secrets";
      mountPoint = "/microvm-secrets";
    }
    {
      proto = "virtiofs";
      tag = "keep";
      source = "/var/lib/microvms/${hostName}/keep";
      mountPoint = "/keep";
    }
  ];

  home-manager = {
    users.${adminUser.name} = {
      imports = [../../users/profiles/headless.nix];
    };
  };
}
