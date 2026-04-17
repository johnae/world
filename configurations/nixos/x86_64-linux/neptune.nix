{
  adminUser,
  hostName,
  config,
  lib,
  pkgs,
  ...
}: {
  age.rekey = {
    hostPubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMwjseLt2hx6kxZUpC/kh0Iy3E0gH5bSFdoy8awcbTei";
    storageMode = "local";
    localStorageDir = ../../../secrets/rekeyed + "/${hostName}";
  };
  syncthingDeviceID = "7B3B6IC-6GMJKDX-YEZD2Q3-X23MNVQ-QHPJM6F-RI46BGJ-M7GWWFF-3VBNIQC";

  ephemeralRoot = true;
  imports = [
    ../../../profiles/admin-user/home-manager.nix
    ../../../profiles/admin-user/u2fmappings.nix
    ../../../profiles/admin-user/user.nix
    ../../../profiles/disk/disko-btrfs-raid.nix
    ../../../profiles/core-metrics.nix
    ../../../profiles/core-logging.nix
    ../../../profiles/greetd.nix
    # ../../../profiles/hardware/usbcore.nix
    ../../../profiles/hardware/b550.nix
    ../../../profiles/home-manager.nix
    ../../../profiles/interception-tools.nix
    ../../../profiles/laptop.nix
    ../../../profiles/restic-backup.nix
    ../../../profiles/state.nix
    ../../../profiles/syncthing.nix
    ../../../profiles/tailscale.nix
    ../../../profiles/zram.nix
  ];

  disko.devices.disk.disk1.device = "/dev/disk/by-path/pci-0000:c1:00.0-nvme-1";
  disko.devices.disk.disk2.device = "/dev/disk/by-path/pci-0000:c2:00.0-nvme-1";

  services.ollama.enable = true;
  services.ollama.rocmOverrideGfx = "11.0.0"; ## rdna 3 11.0.0
  services.ollama.package = pkgs.ollama-rocm;
  services.ollama.host = "0.0.0.0";

  services.sunshine = {
    enable = true;
    autoStart = true;
    capSysAdmin = false;
    openFirewall = true;
  };

  boot.initrd = {
    systemd.enable = true;
    systemd.emergencyAccess = config.users.users.${adminUser.name}.hashedPassword;
    luks.devices.encrypted1.crypttabExtraOpts = ["tpm2-device=auto" "fido2-device=auto"];
    luks.devices.encrypted2.crypttabExtraOpts = ["tpm2-device=auto" "fido2-device=auto"];
  };

  networking.useDHCP = false;

  networking.nameservers = lib.mkForce [];
  services.resolved = {
    enable = true;
    ## have fallbacks in case something is wrong
    settings.Resolve.FallbackDNS = ["9.9.9.9" "149.112.112.112" "2620:fe::fe" "2620:fe::9"];
    ## for some reason, systemd-resolved thinks upstream doesn't respond sometimes
    ## so we need to disable caching negative responses (plus some other stuff)
    ## again - this is about using tailscale dns only
    settings.Resolve.DNSSEC = false;
    settings.Resolve.DNSOverTLS = false;
    settings.Resolve.Cache = "no-negative";
  };

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

  age.secrets = {
    wifi-networks = {
      rekeyFile = ../../../secrets/wifi-networks.age;
    };
    copilot-token = {
      rekeyFile = ../../../secrets/gh_copilot.age;
      owner = "${toString adminUser.uid}";
      path = "/home/${adminUser.name}/.config/github-copilot/hosts.json";
    };
    id_ed25519_alt = {
      rekeyFile = ../../../secrets/id_ed25519_alt.age;
      owner = "${toString adminUser.uid}";
      path = "/home/${adminUser.name}/.ssh/id_ed25519_alt";
    };
    id_ed25519_bbph = {
      rekeyFile = ../../../secrets/id_ed25519_bbph.age;
      owner = "${toString adminUser.uid}";
      path = "/home/${adminUser.name}/.ssh/id_ed25519_bbph";
    };
    id_ed25519_ev = {
      rekeyFile = ../../../secrets/id_ed25519_ev.age;
      owner = "${toString adminUser.uid}";
      path = "/home/${adminUser.name}/.ssh/id_ed25519_ev";
    };
    ssh_host_neptune_ed25519_key = {
      rekeyFile = ../../../secrets/${hostName}/id_ed25519_host_key.age;
    };
    ssh_initrd_neptune_ed25519_key = {
      rekeyFile = ../../../secrets/${hostName}/id_ed25519_initrd_key.age;
    };
    id_rsa_alt = {
      rekeyFile = ../../../secrets/id_rsa_alt.age;
      owner = "${toString adminUser.uid}";
      path = "/home/${adminUser.name}/.ssh/id_rsa_alt";
    };
    id_ed25519_alt_root = {
      rekeyFile = ../../../secrets/id_ed25519_alt.age;
      owner = "0";
      path = "/root/.ssh/id_ed25519";
    };
    age_key_ev = {
      rekeyFile = ../../../secrets/age-key-ev.age;
      owner = "${toString adminUser.uid}";
      path = "/home/${adminUser.name}/.age/key.txt";
    };
    syncthing-cert = {
      rekeyFile = ../../../secrets/${hostName}/syncthing-cert.age;
      owner = "${toString adminUser.uid}";
    };
    syncthing-key = {
      rekeyFile = ../../../secrets/${hostName}/syncthing-key.age;
      owner = "${toString adminUser.uid}";
    };
    groq-api-key = {
      rekeyFile = ../../../secrets/groq-api-key.age;
      owner = "${toString adminUser.uid}";
    };
    openrouter-api-key = {
      rekeyFile = ../../../secrets/openrouter-api-key.age;
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
          "cygnus"
          "eris"
          "icarus"
          "s8plus"
          "z6fold"
        ];
      };
      folders."/home/${adminUser.name}/Pictures" = {
        id = "pictures";
        devices = [
          "eris"
        ];
      };
      folders."/home/${adminUser.name}/Photos" = {
        id = "photos";
        devices = [
          "cygnus"
          "eris"
          "icarus"
          "z6fold"
        ];
        versioning.type = "staggered";
        versioning.params.cleanInterval = "3600";
        versioning.params.maxAge = "0";
        versioning.params.versionsPath = "/home/${adminUser.name}/Photos/stbackup";
      };
    };
  };

  users.users.${adminUser.name}.extraGroups = ["render" "video" "uinput"];

  home-manager = {
    users.${adminUser.name} = {
      imports = [
        ../../../users/profiles/workstation.nix
        ../../../users/profiles/mail.nix
        ../../../users/profiles/9k.nix
      ];
      programs.git.settings.user.signingKey = config.age.secrets.id_ed25519_alt.path;
      programs.jujutsu.settings.signing = {
        behavior = "own";
        backend = "ssh";
        key = config.age.secrets.id_ed25519_alt.path;
      };
    };
  };
}
