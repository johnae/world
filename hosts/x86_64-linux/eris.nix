{
  adminUser,
  hostName,
  ...
}: {
  publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKW2RBUkd9iB3Gs2Krxi9aB3XZE78Qd3RPvA8emXtCpD";
  syncthingDeviceID = "ZL2OU7Q-IRYWF7H-VNX6A65-RHWQXIF-AK2G22Y-KB43E2U-DAMNCFP-66IWKQX";

  imports = [
    ../../profiles/hardware/usbcore.nix
    ../../profiles/hardware/x570.nix
    ../../profiles/disk/bcachefs.nix
    ../../profiles/admin-user/user.nix
    ../../profiles/admin-user/u2fmappings.nix
    ../../profiles/admin-user/home-manager.nix
    ../../profiles/desktop.nix
    ../../profiles/greetd.nix
    ../../profiles/home-manager.nix
    ../../profiles/interception-tools.nix
    ../../profiles/pamu2f.nix
    ../../profiles/restic-backup.nix
    ../../profiles/state.nix
    ../../profiles/syncthing.nix
    ../../profiles/tailscale.nix
    ../../profiles/zram.nix
  ];

  boot.loader.systemd-boot.memtest86.enable = true;

  bcachefs.disks = ["/dev/nvme0n1"];

  boot.initrd = {
    systemd.enable = true;
    #    luks.devices.cryptkey.crypttabExtraOpts = ["fido2-device=auto"];
  };

  age.secrets = {
    spotnix = {
      file = ../../secrets/spotnix.age;
      owner = "${toString adminUser.uid}";
    };
    spotifyd = {
      file = ../../secrets/spotifyd.age;
      owner = "${toString adminUser.uid}";
    };
    wifi-networks = {
      file = ../../secrets/wifi-networks.age;
    };
    id_ed25519_agenda_updater = {
      file = ../../secrets/id_ed25519_agenda_updater.age;
      owner = "${toString adminUser.uid}";
    };
    id_ed25519_roam_updater = {
      file = ../../secrets/id_ed25519_roam_updater.age;
      owner = "${toString adminUser.uid}";
    };
    id_ed25519_bbph = {
      file = ../../secrets/id_ed25519_bbph.age;
      owner = "${toString adminUser.uid}";
      path = "/home/${adminUser.name}/.ssh/id_ed25519_bbph";
    };
    id_ed25519_alt = {
      file = ../../secrets/id_ed25519_alt.age;
      owner = "${toString adminUser.uid}";
      path = "/home/${adminUser.name}/.ssh/id_ed25519_alt";
    };
    id_rsa_alt = {
      file = ../../secrets/id_rsa_alt.age;
      owner = "${toString adminUser.uid}";
      path = "/home/${adminUser.name}/.ssh/id_rsa_alt";
    };
    id_ed25519_alt_root = {
      file = ../../secrets/id_ed25519_alt.age;
      owner = "0";
      path = "/root/.ssh/id_ed25519";
    };
    syncthing-cert = {
      file = ../../secrets/${hostName}/syncthing-cert.age;
      owner = "${toString adminUser.uid}";
    };
    syncthing-key = {
      file = ../../secrets/${hostName}/syncthing-key.age;
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
          "icarus"
          "polaris"
          "titan"
          "hyperion"
          "s23ultra"
          "s8plus"
        ];
      };
      folders."/home/${adminUser.name}/Pictures" = {
        id = "pictures";
        devices = [
          "antares"
          "polaris"
        ];
      };
      folders."/home/${adminUser.name}/Photos" = {
        id = "photos";
        devices = [
          "antares"
          "icarus"
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

  programs.steam.enable = true;

  home-manager = {
    users.${adminUser.name} = {
      imports = [../../users/profiles/workstation.nix];
      programs.git.extraConfig.user.signingKey = "key::sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIAwJWtQ5ZU9U0szWzJ+/GH2uvXZ15u9lL0RdcHdsXM0VAAAABHNzaDo=";
    };
  };
}
