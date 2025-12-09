{
  adminUser,
  config,
  hostName,
  pkgs,
  ...
}: {
  publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIC4ZLSIZTXhSoaKyXhZ3/hBKDXdJx/Yx0lCAhMyyDhRR";

  jovian.steam.enable = true;
  jovian.steam.autoStart = true;
  jovian.steam.user = adminUser.name;
  jovian.hardware.has.amd.gpu = true;

  ephemeralRoot = true;
  imports = [
    ../../../profiles/admin-user/home-manager.nix
    ../../../profiles/admin-user/user.nix
    ../../../profiles/defaults.nix
    ../../../profiles/disk/disko-btrfs-workstation.nix
    ../../../profiles/hardware/b550.nix
    ../../../profiles/home-manager.nix
    ../../../profiles/workstation.nix
    ../../../profiles/state.nix
    ../../../profiles/tailscale.nix
    ../../../profiles/zram.nix
  ];

  disko.devices.disk.disk1.device = "/dev/nvme0n1";

  users.users.${adminUser.name}.linger = true; ## start user systemd units on boot

  system.autoUpgrade = {
    enable = true;
    flake = "github:johnae/world";
    allowReboot = true;
    dates = "06:00";
    randomizedDelaySec = "5min";
    enableSentinel = false; ## not running kubernetes here
  };

  boot.initrd.availableKernelModules = [
    "igc"
    "nvme"
    "ahci"
    "usbhid"
  ];

  boot.initrd = {
    systemd.enable = true;
    systemd.emergencyAccess = config.users.users.${adminUser.name}.hashedPassword;
    luks.devices.encrypted.crypttabExtraOpts = ["tmp2-device=auto" "fido2-device=auto"];
    luks.devices.encrypted-swap.crypttabExtraOpts = ["tpm2-device=auto" "fido2-device=auto"];
  };

  age.secrets = {
    ssh_host_jupiter_ed25519_key = {
      file = ../../../secrets/jupiter/id_ed25519_host_key.age;
    };
    ssh_initrd_jupiter_ed25519_key = {
      file = ../../../secrets/jupiter/id_ed25519_initrd_key.age;
    };
    wifi-networks = {
      file = ../../../secrets/wifi-networks.age;
    };
  };

  services.tailscale.auth = {
    enable = true;
    args.advertise-tags = ["tag:server"];
    args.ssh = true;
    args.accept-routes = false;
    args.accept-dns = false;
    args.advertise-exit-node = true;
    args.auth-key = "file:/keep/etc/tailscale-auth-key";
  };

  networking.useDHCP = false;
  networking.firewall.trustedInterfaces = ["tailscale0"];

  systemd.network = {
    enable = true;
    wait-online.anyInterface = true;
    networks = {
      "10-lan" = {
        matchConfig.Name = ["enp*" "wlan*" "wlp*"];
        networkConfig.DHCP = "yes";
        networkConfig.IgnoreCarrierLoss = "3s";
        networkConfig.IPv6PrivacyExtensions = "yes";
        networkConfig.IPv6AcceptRA = "yes";
      };
    };
  };

  home-manager.users.${adminUser.name} = {
    imports = [
      ../../../users/profiles/default.nix
    ];
  };
}
