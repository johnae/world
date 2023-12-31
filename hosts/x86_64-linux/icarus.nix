{
  adminUser,
  hostName,
  ...
}: {
  publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMfXrbXEQcJ85CGW9/qnm/jS8Vp6GP2yQ2piFF8+OFWR";
  syncthingDeviceID = "HBL5ZRB-R2STGW5-LMAYYHX-KOFTP3X-VO4IV6E-PEDKZ3N-WCRR7BY-F5C7AAP";

  btrfs.disks = ["/dev/nvme0n1" "/dev/nvme1n1"];

  imports = [
    ../../profiles/admin-user/home-manager.nix
    ../../profiles/admin-user/user.nix
    ../../profiles/disk/btrfs-on-luks.nix
    ../../profiles/hardware/b550.nix
    ../../profiles/home-manager.nix
    ../../profiles/server.nix
    ../../profiles/state.nix
    ../../profiles/syncthing.nix
    ../../profiles/tailscale.nix
    ../../profiles/uuid_disk_crypt.nix
    ../../profiles/zram.nix
  ];

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

  networking = {
    defaultGateway = "192.168.20.1";
    firewall.trustedInterfaces = ["tailscale0"];
    interfaces.eth0.ipv4.addresses = [
      {
        address = "192.168.20.143";
        prefixLength = 24;
      }
    ];
  };

  age.secrets = {
    syncthing-cert = {
      file = ../../secrets/${hostName}/syncthing-cert.age;
      owner = "${toString adminUser.uid}";
    };
    syncthing-key = {
      file = ../../secrets/${hostName}/syncthing-key.age;
      owner = "${toString adminUser.uid}";
    };
    ts-google-9k = {
      file = ../../secrets/ts-google-9k.age;
      owner = "${toString adminUser.uid}";
    };
  };

  services.syncthing = {
    enable = true;
    user = "${adminUser.name}";
    group = "users";
    openDefaultPorts = true;
    cert = "/run/agenix/syncthing-cert";
    key = "/run/agenix/syncthing-key";
    dataDir = "/home/${adminUser.name}/.local/share/syncthing-data";

    settings = {
      devices.s23ultra.id = "WIEQUVJ-5TNGRPN-YD4E47D-WEXXBZO-2AGHFQZ-2K4DDRB-DFSD2UZ-34OCBQ4";
      devices.s8plus.id = "EI6DXMZ-3CMM3R3-LNJPFIF-CTXDVAG-2SXLOCY-4NEEZ3K-CYJBXU6-6W44TAV";
      folders."/home/${adminUser.name}/Sync" = {
        id = "sync";
        devices = [
          "antares"
          "eris"
          "hyperion"
          "polaris"
          "s23ultra"
          "s8plus"
          "titan"
        ];
      };
      folders."/home/${adminUser.name}/Photos" = {
        id = "photos";
        devices = [
          "antares"
          "eris"
          "polaris"
          "s23ultra"
        ];
        versioning.type = "staggered";
        versioning.params.cleanInterval = "3600";
        versioning.params.maxAge = "0";
        versioning.params.versionsPath = "/home/${adminUser.name}/Photos/stbackup";
      };
    };
  };

  home-manager = {
    users.${adminUser.name} = {
      imports = [../../users/profiles/headless.nix];
    };
  };
}
