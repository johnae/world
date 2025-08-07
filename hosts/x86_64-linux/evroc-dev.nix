{
  adminUser,
  lib,
  ...
}: {
  publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDErC2NyMr7hmuNA9gnuLveTxPjYVqkmpLL9j6kzf2a5";

  imports = [
    ../../profiles/admin-user/home-manager.nix
    ../../profiles/admin-user/user.nix
    ../../profiles/home-manager.nix
    ../../profiles/hcloud.nix
    ../../profiles/hcloud-remote-unlock.nix
    ../../profiles/tailscale.nix
    ../../profiles/zram.nix
  ];

  networking.firewall.trustedInterfaces = ["tailscale0"];

  services.tailscale.auth = {
    enable = true;
    after = ["metadata.service"];
    args.advertise-tags = ["tag:server"];
    args.ssh = true;
    args.accept-routes = false;
    args.accept-dns = true;
    args.advertise-exit-node = true;
    args.auth-key = "file:/etc/tailscale-auth-key";
  };

  boot.initrd.network.ssh.authorizedKeys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ266FsDSw6v4gU9PwSun1aLKpS/BML4QOB1Cii9y1dM" ## gh
  ];

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIzm5RyD+1nfy1LquvkEog4SZtPgdhzjr49jSC8PAinp"
  ];

  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
    users.${adminUser.name} = {
      imports = [../../users/profiles/headless.nix];
    };
  };

  disko.devices = {
    disk.disk1 = {
      device = lib.mkDefault "/dev/vda";
      type = "disk";
      content = {
        type = "gpt";
        partitions = {
          boot = {
            name = "boot";
            size = "1M";
            type = "EF02";
          };
          esp = {
            name = "ESP";
            size = "500M";
            type = "EF00";
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot";
            };
          };
          luks = {
            size = "100%";
            content = {
              type = "luks";
              name = "encrypted";
              settings.allowDiscards = true;
              passwordFile = "/tmp/disk.key";
              content = {
                type = "filesystem";
                format = "bcachefs";
                mountpoint = "/";
                mountOptions = ["defaults" "compression=zstd" "background_compression=zstd"];
              };
            };
          };
        };
      };
    };
  };
}
