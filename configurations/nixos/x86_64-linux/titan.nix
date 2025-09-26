{
  adminUser,
  hostName,
  config,
  ...
}: {
  publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOteEiVr+H3Q1tSw1bGQfjbVWuPggc/+w1vVXSFde0Rt";
  syncthingDeviceID = "53R5CPY-RVICTGR-AEXNKZU-RMCKC32-JNON7PH-4VEFCRB-KRPLP3M-AIEQPA6";

  imports = [
    ../../../profiles/admin-user/home-manager.nix
    ../../../profiles/admin-user/user.nix
    ../../../profiles/disk/btrfs-on-luks.nix
    ../../../profiles/hardware/nuc.nix
    ../../../profiles/home-manager.nix
    ../../../profiles/server.nix
    ../../../profiles/state.nix
    ../../../profiles/syncthing.nix
    ../../../profiles/tailscale.nix
    ../../../profiles/uuid_disk_crypt.nix
    ../../../profiles/zram.nix
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

  boot.initrd.availableKernelModules = [
    "e1000e"
    "nvme"
    "ahci"
    "usbhid"
  ];

  services.tailscale.auth = {
    enable = true;
    args.advertise-tags = ["tag:server"];
    args.ssh = true;
    args.accept-routes = false;
    args.accept-dns = false;
    args.auth-key = "file:${config.age.secrets.ts-google-9k.path}";
  };

  networking = {
    defaultGateway = "192.168.20.1";
    firewall.trustedInterfaces = ["tailscale0"];
    interfaces.eth0.ipv4.addresses = [
      {
        address = "192.168.20.142";
        prefixLength = 24;
      }
    ];
  };

  age.secrets = {
    remote-disk-password.file = ../../../secrets/remote-disk-password.age;
    remote-cloud-disk-password.file = ../../../secrets/remote-cloud-disk-password.age;
    id_ed25519_remote_unlock.file = ../../../secrets/${hostName}/id_ed25519_remote_unlock.age;
    syncthing-cert = {
      file = ../../../secrets/${hostName}/syncthing-cert.age;
      owner = "${toString adminUser.uid}";
    };
    syncthing-key = {
      file = ../../../secrets/${hostName}/syncthing-key.age;
      owner = "${toString adminUser.uid}";
    };
    ts-google-9k = {
      file = ../../../secrets/ts-google-9k.age;
      owner = "${toString adminUser.uid}";
    };
    hcloud-token = {
      file = ../../../secrets/hcloud-token.age;
      owner = "${toString adminUser.uid}";
    };
  };

  services.syncthing = {
    enable = true;
    user = "${adminUser.name}";
    group = "users";
    openDefaultPorts = true;
    cert = config.age.secrets.syncthing-cert.path;
    key = config.age.secrets.syncthing-key.path;
    dataDir = "/home/${adminUser.name}/.local/share/syncthing-data";

    settings = {
      devices.s8plus.id = "EI6DXMZ-3CMM3R3-LNJPFIF-CTXDVAG-2SXLOCY-4NEEZ3K-CYJBXU6-6W44TAV";
      devices.z6fold.id = "2HBWA7C-4MR7BQQ-5JGQHNE-W7NBEY6-W6LAQQX-M52KWWD-JEAOZDJ-SKBBLAD";
      folders."/home/${adminUser.name}/Sync" = {
        id = "sync";
        devices = [
          "antares"
          "eris"
          "hyperion"
          "icarus"
          "sirius"
          "s8plus"
          "z6fold"
        ];
      };
    };
  };

  services.remote-unlock = [
    {
      enable = true;
      host = "65.109.85.161";
      port = 2222;
      identityFile = config.age.secrets.id_ed25519_remote_unlock.path;
      passwordFile = config.age.secrets.remote-disk-password.path;
    }
    {
      enable = true;
      host = "65.109.92.173";
      port = 2222;
      identityFile = config.age.secrets.id_ed25519_remote_unlock.path;
      passwordFile = config.age.secrets.remote-disk-password.path;
    }
    {
      enable = true;
      host = "144.76.201.232";
      port = 2222;
      identityFile = config.age.secrets.id_ed25519_remote_unlock.path;
      passwordFile = config.age.secrets.remote-disk-password.path;
    }
  ];

  services.hcloud-remote-unlock-all = {
    enable = true;

    hcloudTokenFile = config.age.secrets.hcloud-token.path;
    identityFile = config.age.secrets.id_ed25519_remote_unlock.path;
    diskpasswordFile = config.age.secrets.remote-cloud-disk-password.path;
  };

  home-manager = {
    users.${adminUser.name} = {
      imports = [../../../users/profiles/headless.nix];
    };
  };
}
