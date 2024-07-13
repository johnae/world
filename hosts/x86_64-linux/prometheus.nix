{
  adminUser,
  hostName,
  pkgs,
  ...
}: {
  publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBSg05Gm7aFf2DpPLNPbnXfFnCpjKVtE5svM6wFtnTYJ";
  syncthingDeviceID = "2CJB6EV-IMHV3H7-O3BLFCM-CMCXTIV-6WX2LZ5-NLZ4XWV-YXPNZF5-66ZPIQT";

  bcachefs = {
    disks = ["/dev/nvme0n1" "/dev/nvme1n1"];
    devices = ["/dev/mapper/encrypted_root" "/dev/mapper/encrypted_root1"];
  };

  imports = [
    ../../profiles/acme.nix
    ../../profiles/admin-user/home-manager.nix
    ../../profiles/admin-user/user.nix
    ../../profiles/disk/bcachefs-on-luks.nix
    ../../profiles/hardware/gex44.nix
    ../../profiles/home-manager.nix
    ../../profiles/server.nix
    ../../profiles/restic-backup.nix
    ../../profiles/state.nix
    ../../profiles/syncthing.nix
    ../../profiles/tailscale.nix
    ../../profiles/zram.nix
  ];

  virtualisation = {
    docker.enable = false;
    podman.enable = true;
    podman.dockerCompat = true;
    libvirtd.enable = true;
  };

  hardware.graphics.enable = true;

  hardware.graphics.extraPackages = [
    pkgs.intel-media-driver
    pkgs.vaapiIntel
    pkgs.vaapiVdpau
    pkgs.libvdpau-va-gl
    # pkgs.rocmPackages.clr
    # pkgs.rocmPackages.clr.icd
    pkgs.amdvlk
  ];
  services.xserver.videoDrivers = ["nvidia"];
  hardware.nvidia.nvidiaPersistenced = true;
  hardware.nvidia.prime.nvidiaBusId = "PCI:1:0:0";
  environment.systemPackages = [
    pkgs.cudaPackages.cudatoolkit
  ];

  # microvm.autostart = [
  #   "agent-8be5-d4a1"
  #   "agent-8be5-15c3"
  #   "agent-8be5-32c4"
  #   "master-8be5-a0a1"
  #   "master-8be5-c1ce"
  # ];

  programs.ssh.startAgent = true;

  boot.binfmt.emulatedSystems = ["aarch64-linux"];

  # services.nextjs-ollama-llm-ui.enable = true;
  # services.nextjs-ollama-llm-ui.port = 1337;
  # services.nextjs-ollama-llm-ui.ollamaUrl = "http://100.123.66.6:11434";
  services.ollama.enable = true;
  services.ollama.acceleration = "cuda";
  services.ollama.host = "0.0.0.0";
  services.ollama.environmentVariables = {
    OLLAMA_FLASH_ATTENTION = "True";
    CUDA_ERROR_LEVEL = "50";
  };

  system.autoUpgrade = {
    enable = true;
    flake = "github:johnae/world";
    allowReboot = true;
    dates = "06:00";
    randomizedDelaySec = "5min";
    enableSentinel = false; ## not running kubernetes here
  };

  boot.kernelParams = [
    "ip=144.76.201.232::144.76.201.225:255.255.255.224:${hostName}::none"
  ];

  ## for tailscale exit node functionality
  boot.kernel.sysctl."net.ipv4.ip_forward" = 1;

  boot.initrd.network = {
    enable = true;
    postCommands = "echo 'cryptsetup-askpass' >> /root/.profile";
    flushBeforeStage2 = true;
    ssh = {
      enable = true;
      port = 2222;
      hostKeys = [
        "/keep/secrets/initrd_ed25519_key"
      ];
      authorizedKeys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCyjMuNOFrZBi7CrTyu71X+aRKyzvTmwCEkomhB0dEhENiQ3PTGVVWBi1Ta9E9fqbqTW0HmNL5pjGV+BU8j9mSi6VxLzJVUweuwQuvqgAi0chAJVPe0FSzft9M7mJoEq5DajuSiL7dSjXpqNFDk/WCDUBE9pELw+TXvxyQpFO9KZwiYCCNRQY6dCjrPJxGwG+JzX6l900GFrgOXQ3KYGk8vzep2Qp+iuH1yTgEowUICkb/9CmZhHQXSvq2gAtoOsGTd9DTyLOeVwZFJkTL/QW0AJNRszckGtYdA3ftCUNsTLSP/VqYN9EjxcMHQe4PGjkK7VLb59DQJFyRQqvPXiUyxNloHcu/sDuiKHIk/0qDLHlVn2xc5zkvzSqoQxoXx+P4dDbje1KHLY8E96gLe2Csu0ti+qsM5KEvgYgwWwm2g3IBlaWwgAtC0UWEzIuBPrAgPd5vi+V50ITIaIk6KIV7JPOubLUXaLS5KW77pWyi9PqAGOXj+DgTWoB3QeeZh7CGhPL5fAecYN7Pw734cULZpnw10Bi/jp4Nlq1AJDk8BwLUJbzZ8aexwMf78syjkHJBBrTOAxADUE02nWBQd0w4K5tl/a3UnBYWGyX8TD44046Swl/RY/69PxFvYcVRuF4eARI6OWojs1uhoR9WkO8eGgEsuxxECwNpWxR5gjKcgJQ=="
        "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIJY3QSBIiRKN8/B3nHgCBDp;auQBOftphOeuF2TaBHGQSAAAABHNzaDo="
        "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIAwJWtQ5ZU9U0szWzJ+/GH2uvXZ15u9lL0RdcHdsXM0VAAAABHNzaDo="
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK+trOinD68RD1efI6p05HeaNA0SjzeRnUvpf22+jsq+"
      ];
    };
  };

  services.tailscale = {
    authKeyFile = "/var/run/agenix/ts-google-9k";
    useRoutingFeatures = "both";
    extraUpFlags = [
      "--accept-dns=true"
      "--accept-routes=true"
      "--advertise-exit-node=true"
      "--ssh"
    ];
  };

  networking.useDHCP = false;
  networking.nat = {
    enable = true;
    enableIPv6 = true;
    internalInterfaces = ["microvm"];
  };

  services.networkd-dispatcher = let
    tailscale-forwarding = pkgs.writeShellApplication {
      name = "tailscale-forwarding";
      runtimeInputs = with pkgs; [iproute2 ethtool coreutils];
      text = ''
        for dev in $(ip route show 0/0 | cut -f5 -d' '); do echo ethtool -K "$dev" rx-udp-gro-forwarding on rx-gro-list off; done
      '';
    };
  in {
    enable = true;
    rules = {
      "50-tailscale" = {
        onState = ["routable"];
        script = "${tailscale-forwarding}/bin/tailscale-forwarding";
      };
    };
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
      "10-wan" = {
        ## udevadm test-builtin net_id /sys/class/net/eth0
        ## https://www.freedesktop.org/software/systemd/man/latest/systemd.net-naming-scheme.html
        matchConfig.Name = ["enp*"];
        address = [
          "144.76.201.232/27"
          "2a01:4f8:200:82c5::2/64"
        ];
        routes = [
          {routeConfig.Gateway = "144.76.201.225";}
          {routeConfig.Gateway = "fe80::1";}
        ];
        linkConfig.RequiredForOnline = "routable";
      };
      "10-microvm" = {
        matchConfig.Name = "microvm";
        networkConfig = {
          DHCPServer = true;
          IPv6SendRA = true;
        };
        addresses = [
          {
            addressConfig.Address = "10.100.2.1/24";
          }
        ];
      };
      "11-microvm" = {
        matchConfig.Name = "vm-*";
        networkConfig.Bridge = "microvm";
      };
    };
  };

  networking.firewall.trustedInterfaces = ["tailscale0" "microvm"];

  age.secrets = {
    initrd-key = {
      file = ../../secrets/${hostName}/initrd_ed25519_key.age;
      owner = "${toString adminUser.uid}";
    };
    host-key = {
      file = ../../secrets/${hostName}/host_ed25519_key.age;
      owner = "${toString adminUser.uid}";
    };
    copilot-token = {
      file = ../../secrets/gh_copilot.age;
      owner = "${toString adminUser.uid}";
      path = "/home/${adminUser.name}/.config/github-copilot/hosts.json";
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
    ssh_host_microvm_ed25519_key = {
      file = ../../secrets/ssh_host_microvm_ed25519_key.age;
    };
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
          "orion"
          "sirius"
          "icarus"
          "s23ultra"
          "s8plus"
          "titan"
        ];
      };
      folders."/home/${adminUser.name}/Pictures" = {
        id = "pictures";
        devices = [
          "antares"
          "sirius"
          "orion"
          "eris"
        ];
      };
    };
  };

  home-manager = {
    users.${adminUser.name} = {
      imports = [../../users/profiles/headless.nix];
      programs.git.extraConfig.user.signingKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIzm5RyD+1nfy1LquvkEog4SZtPgdhzjr49jSC8PAinp";
      home.sessionVariables = {
        SSH_AUTH_SOCK = "/run/user/${toString adminUser.uid}/ssh-auth.sock";
      };
    };
  };
}
