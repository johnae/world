{
  adminUser,
  hostName,
  config,
  ...
}: {
  publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPI8D16MUXSt7EcmYavSBD6k1eHoIoFfH4lu8T5o+4zX";
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

  services.ollama.enable = true;
  services.ollama.rocmOverrideGfx = "11.0.0"; ## rdna 3 11.0.0
  services.ollama.acceleration = "rocm";

  boot.loader.systemd-boot.memtest86.enable = true;

  bcachefs.disks = ["/dev/nvme0n1"];
  bcachefs.devices = ["/dev/nvme0n1p3"];

  boot.initrd = {
    systemd.enable = true;
    systemd.emergencyAccess = config.users.users.${adminUser.name}.hashedPassword;
  };

  networking.useDHCP = false;
  networking.nat = {
    enable = true;
    enableIPv6 = true;
    internalInterfaces = ["microvm"];
  };
  systemd.network = {
    enable = true;
    wait-online.anyInterface = true;
    netdevs = {
      "10-microvm".netdevConfig = {
        Kind = "bridge";
        Name = "microvm";
      };
    };
    networks = {
      "10-lan" = {
        matchConfig.Name = ["enp*" "wlan*" "wlp*"];
        networkConfig.DHCP = "ipv4";
      };
      "10-microvm" = {
        matchConfig.Name = "microvm";
        networkConfig = {
          DHCPServer = true;
          IPv6SendRA = true;
        };
        addresses = [
          {
            Address = "10.100.100.1/24";
          }
        ];
      };
      "11-microvm" = {
        matchConfig.Name = "vm-*";
        networkConfig.Bridge = "microvm";
      };
    };
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
    groq-api-key = {
      file = ../../secrets/groq-api-key.age;
      owner = "${toString adminUser.uid}";
    };
    anthropic-api-key = {
      file = ../../secrets/anthropic-api-key.age;
      owner = "${toString adminUser.uid}";
    };
    openrouter-api-key = {
      file = ../../secrets/openrouter-api-key.age;
      owner = "${toString adminUser.uid}";
    };
    openai-api-key = {
      file = ../../secrets/openai-api-key.age;
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
          "icarus"
          "cygnus"
          "sirius"
          "orion"
          "titan"
          "hyperion"
          "s8plus"
          "z6fold"
        ];
      };
      folders."/home/${adminUser.name}/Pictures" = {
        id = "pictures";
        devices = [
          "antares"
          "cygnus"
          "orion"
          "sirius"
        ];
      };
      folders."/home/${adminUser.name}/Photos" = {
        id = "photos";
        devices = [
          "antares"
          "icarus"
          "cygnus"
          "orion"
          "sirius"
          "z6fold"
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
      programs.git.extraConfig.user.signingKey = config.age.secrets.id_ed25519_alt.path;
      programs.jujutsu.settings.signing = {
        sign-all = true;
        backend = "ssh";
        key = config.age.secrets.id_ed25519_alt.path;
      };
    };
  };
}
