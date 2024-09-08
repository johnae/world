{
  adminUser,
  hostName,
  config,
  ...
}: {
  publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKEH+uJLUMT2PZ3AK+UNTiyc/MS58mXMyjxLFtdeN114";
  syncthingDeviceID = "7HYSCXB-KGNIK4Y-24SOMMD-CYKJLYR-GLK7D3R-NWJFSYM-ZCBRNF2-KQJ7ZQW";

  imports = [
    ../../profiles/admin-user/home-manager.nix
    ../../profiles/admin-user/u2fmappings.nix
    ../../profiles/admin-user/user.nix
    ../../profiles/disk/bcachefs.nix
    ../../profiles/greetd.nix
    ../../profiles/hardware/usbcore.nix
    ../../profiles/hardware/framework-13-amd.nix
    ../../profiles/home-manager.nix
    ../../profiles/interception-tools.nix
    ../../profiles/laptop.nix
    ../../profiles/restic-backup.nix
    ../../profiles/state.nix
    ../../profiles/syncthing.nix
    ../../profiles/tailscale.nix
    ../../profiles/zram.nix
  ];

  bcachefs.disks = ["/dev/nvme0n1"];
  bcachefs.devices = ["/dev/nvme0n1p3"];

  boot.initrd = {
    systemd.enable = true;
    #luks.devices.cryptkey.crypttabExtraOpts = ["fido2-device=auto"];
  };

  age.secrets = {
    wifi-networks = {
      file = ../../secrets/wifi-networks.age;
    };
    copilot-token = {
      file = ../../secrets/gh_copilot.age;
      owner = "${toString adminUser.uid}";
      path = "/home/${adminUser.name}/.config/github-copilot/hosts.json";
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
    groq-lsp-ai = {
      file = ../../secrets/groq-lsp-ai.age;
      owner = "${toString adminUser.uid}";
    };
    anthropic-lsp-ai = {
      file = ../../secrets/anthropic-lsp-ai.age;
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
        ];
      };
      folders."/home/${adminUser.name}/Photos" = {
        id = "photos";
        devices = [
          "eris"
          "icarus"
          "orion"
          "sirius"
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
