{
  adminUser,
  hostName,
  ...
}: {
  publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJgkTCs3JTN934N8RkPhuDwijFgFOEXt734tB7aQz57Z";
  syncthingDeviceID = "CI5DK5E-JLWEOXB-6TUV244-SV6MLGO-R6AY7XQ-ZOFCIQW-G3U2DAS-BK5EXAL";

  imports = [
    ../../profiles/admin-user/home-manager.nix
    ../../profiles/admin-user/u2fmappings.nix
    ../../profiles/admin-user/user.nix
    ../../profiles/disk/btrfs-on-luks.nix
    ../../profiles/greetd.nix
    ../../profiles/hardware/usbcore.nix
    ../../profiles/hardware/z16.nix
    ../../profiles/home-manager.nix
    ../../profiles/interception-tools.nix
    ../../profiles/laptop.nix
    ../../profiles/pamu2f.nix
    ../../profiles/restic-backup.nix
    ../../profiles/state.nix
    ../../profiles/syncthing.nix
    ../../profiles/tailscale.nix
    ../../profiles/zram.nix
  ];

  boot.initrd = {
    systemd.enable = true;
    luks.devices.cryptkey.crypttabExtraOpts = ["fido2-device=auto"];
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
    copilot-token = {
      file = ../../secrets/gh_copilot.age;
      owner = "${toString adminUser.uid}";
      path = "/home/${adminUser.name}/.config/github-copilot/hosts.json";
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
      devices.z6fold.id = "2HBWA7C-4MR7BQQ-5JGQHNE-W7NBEY6-W6LAQQX-M52KWWD-JEAOZDJ-SKBBLAD";
      folders."/home/${adminUser.name}/Sync" = {
        id = "sync";
        devices = [
          "eris"
          "icarus"
          "sirius"
          "orion"
          "cygnus"
          "titan"
          "hyperion"
          "s23ultra"
          "s8plus"
          "z6fold"
        ];
      };
      folders."/home/${adminUser.name}/Pictures" = {
        id = "pictures";
        devices = [
          "eris"
          "orion"
          "sirius"
          "cygnus"
        ];
      };
      folders."/home/${adminUser.name}/Photos" = {
        id = "photos";
        devices = [
          "eris"
          "icarus"
          "orion"
          "sirius"
          "cygnus"
          "s23ultra"
          "z6fold"
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
      imports = [../../users/profiles/workstation.nix];
      programs.git.extraConfig.user.signingKey = config.age.secrets.id_ed25519_alt.path;
    };
  };
}
