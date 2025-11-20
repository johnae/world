{
  adminUser,
  pkgs,
  lib,
  ...
}: {
  publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJnEDS9ziXY31y2uyMBzSFSjyY5KqYnJgR4Tkb8nx/Dn";

  imports = [
    ../../../profiles/admin-user/home-manager.nix
    ../../../profiles/admin-user/user.nix
    ../../../profiles/disk/btrfs-on-luks.nix
    ../../../profiles/hardware/apu.nix
    ../../../profiles/home-manager.nix
    ../../../profiles/server.nix
    ../../../profiles/state.nix
    ../../../profiles/tailscale.nix
    ../../../profiles/uuid_disk_crypt.nix
    ../../../profiles/zram.nix
  ];

  disk.dosLabel = true;

  system.autoUpgrade = {
    enable = true;
    flake = "github:johnae/world";
    allowReboot = true;
    dates = "06:00";
    randomizedDelaySec = "5min";
    enableSentinel = false; ## not running kubernetes here
  };

  ## for tailscale exit node functionality
  boot.kernel.sysctl."net.ipv4.ip_forward" = 1;

  services.tailscale.auth = {
    enable = true;
    args.advertise-tags = ["tag:server"];
    args.ssh = true;
    args.accept-routes = false;
    args.accept-dns = false;
    args.advertise-exit-node = true;
    args.auth-key = "file:/var/run/agenix/ts-google-9k";
  };

  services.jae.router = {
    enable = true;
    upstreamDnsServers = [
      "2620:fe::fe"
      "9.9.9.9"
      "2620:fe::9"
      "149.112.112.112"
    ];
    externalInterface = "enp1s0";
    internalInterface = "enp2s0";
    internalInterfaceIP = "192.168.20.1";
    dnsMasqSettings.no-resolv = true;
    dnsMasqSettings.bogus-priv = true;
    dnsMasqSettings.strict-order = true;
  };

  age.secrets = {
    ts-google-9k = {
      file = ../../../secrets/ts-google-9k.age;
      owner = "1337";
    };
  };

  users.users.${adminUser.name}.shell = lib.mkForce pkgs.bashInteractive;

  home-manager = {
    users.${adminUser.name} = {
      imports = [../../../users/profiles/minimal.nix];
    };
  };
}
