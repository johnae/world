{
  adminUser,
  config,
  hostName,
  lib,
  pkgs,
  ...
}: {
  age.rekey = {
    hostPubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIInNFRkSTXPiS0TiQOBS+AT/N2l5HS5/cnCfbWN1sSik";
    storageMode = "local";
    localStorageDir = ../../../secrets/rekeyed + "/${hostName}";
  };

  ephemeralRoot = true;
  imports = [
    ../../../profiles/hardware/usbcore.nix
    ../../../profiles/hardware/x570.nix
    ../../../profiles/disk/disko-btrfs.nix
    ../../../profiles/admin-user/user.nix
    ../../../profiles/admin-user/u2fmappings.nix
    ../../../profiles/admin-user/home-manager.nix
    ../../../profiles/desktop.nix
    ../../../profiles/greetd.nix
    ../../../profiles/home-manager.nix
    ../../../profiles/interception-tools.nix
    ../../../profiles/pamu2f.nix
    ../../../profiles/supertonic-tts.nix
    ../../../profiles/restic-backup.nix
    ../../../profiles/state.nix
    ../../../profiles/syncthing.nix
    ../../../profiles/tailscale.nix
    ../../../profiles/whisper-cpp.nix
    ../../../profiles/zram.nix
  ];

  ## disable this for now on this machine
  services.logind.settings.Login = {
    IdleAction = lib.mkForce "ignore";
  };

  services.whisper-cpp = {
    enable = true;
    hassApi = "http://icarus:8123/api";
    hassTokenFile = config.age.secrets.whisper-hass-token.path;
    ## Almost everything said here is Swedish and needs no help. What gets
    ## misheard is foreign names asked for by voice, so these are the ones in
    ## recent Spotify listening that whisper cannot know from English alone -
    ## names like Queen or David Bowie it already spells. German album titles
    ## are here for the same reason as the artists.
    initialPrompt = lib.concatStringsSep " " [
      "Spela Schrotthagen, Sturm und Drang, Schubkraft, Ballarak,"
      "Rage Against the Machine, Deep Purple, The Frightnrs, Dolly Style,"
      "Alina Pash, Culture Club, Neon Graveyard, Johannes Schuster,"
      "Guns N' Roses, Led Zeppelin, Baha Men, Bruce Springsteen, Saint Germain,"
      "Erik Satie, Miss Kittin och Blümchen."
    ];
    ## The artists take ~107 tokens and Home Assistant's names ~95, just over
    ## the default 200. Whisper's hard limit is 223.
    promptMaxTokens = 210;
  };

  ## The same read-only token icarus's whisper servers use, so no second
  ## token to issue or revoke.
  age.secrets.whisper-hass-token.rekeyFile = ../../../secrets/icarus/whisper-hass-token.age;

  ## F2 has about 40% more pitch movement than F5, which sounded flat. At 1.25
  ## replies are 16% shorter and still transcribe back word for word.
  services.supertonic-tts = {
    enable = true;
    voice = "F2";
    speed = 1.25;
  };

  services.ollama.enable = true;
  services.ollama.rocmOverrideGfx = "11.0.0"; ## rdna 3 11.0.0
  services.ollama.host = "0.0.0.0";
  ## Gemma writes list arguments as ['sensor'], and ollama's repair for
  ## single-quoted values only looks right after a colon, so the call fails
  ## to parse and Home Assistant gets no reply at all. Rebuilding ollama means
  ## compiling its ROCm kernels, so only for the 7900 XTX's architecture
  ## rather than every AMD GPU nixpkgs targets.
  services.ollama.package =
    (pkgs.ollama-rocm.override {rocmGpuTargets = ["gfx1100"];}).overrideAttrs
    (old: {patches = (old.patches or []) ++ [../../../profiles/ollama-gemma4-single-quoted-lists.patch];});
  ## Keep the model resident. Speech-to-text now shares the GPU, and both fit:
  ## ~14 GiB for the model plus ~4 GiB for kb-whisper-large of the 24. A cold
  ## load costs several seconds on the first question after an idle spell.
  services.ollama.environmentVariables.OLLAMA_KEEP_ALIVE = "-1";

  boot.loader.systemd-boot.memtest86.enable = true;

  ## Single NVMe. disko partitions it as ESP + random-key swap + one luks
  ## volume holding the btrfs subvolumes, same shape as icarus and neptune
  ## minus the second disk.
  disko.devices.disk.disk1.device = "/dev/nvme0n1";

  boot.initrd = {
    systemd.enable = true;
    systemd.emergencyAccess = config.users.users.${adminUser.name}.hashedPassword;
    ## Unlocks itself from the TPM, with a FIDO2 key as the other way in and
    ## the passphrase always behind both. Neither is enrolled by installing -
    ## that is a systemd-cryptenroll run once the machine is up.
    luks.devices.encrypted.crypttabExtraOpts = ["tpm2-device=auto" "fido2-device=auto"];
  };

  networking.useDHCP = false;
  systemd.network = {
    enable = true;
    wait-online.anyInterface = true;
    networks = {
      "10-lan" = {
        matchConfig.Name = ["enp*" "wlan*" "wlp*"];
        networkConfig.DHCP = "ipv4";
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
    id_ed25519_agenda_updater = {
      rekeyFile = ../../../secrets/id_ed25519_agenda_updater.age;
      owner = "${toString adminUser.uid}";
    };
    id_ed25519_roam_updater = {
      rekeyFile = ../../../secrets/id_ed25519_roam_updater.age;
      owner = "${toString adminUser.uid}";
    };
    id_ed25519_bbph = {
      rekeyFile = ../../../secrets/id_ed25519_bbph.age;
      owner = "${toString adminUser.uid}";
      path = "/home/${adminUser.name}/.ssh/id_ed25519_bbph";
    };
    id_ed25519_alt = {
      rekeyFile = ../../../secrets/id_ed25519_alt.age;
      owner = "${toString adminUser.uid}";
      path = "/home/${adminUser.name}/.ssh/id_ed25519_alt";
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
          "antares"
          "cygnus"
          "icarus"
          "neptune"
          "s8plus"
          "z6fold"
        ];
      };
      folders."/home/${adminUser.name}/Pictures" = {
        id = "pictures";
        devices = [
          "antares"
          "cygnus"
          "neptune"
        ];
      };
      folders."/home/${adminUser.name}/Photos" = {
        id = "photos";
        devices = [
          "antares"
          "cygnus"
          "icarus"
          "neptune"
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
      imports = [
        ../../../users/profiles/workstation.nix
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
