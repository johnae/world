{
  adminUser,
  hostName,
  pkgs,
  ...
}: {
  publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPYExvLXmlYzWsoJLST2A9FdzN7re7J+Uz1TMpQ2ndhP";
  syncthingDeviceID = "XFZC5TF-K6CWYRL-GIMFEZO-HNXUS4Q-BPJZJQR-3SVNTXL-X4UABVR-RWDROQD";

  bcachefs = {
    disks = ["/dev/nvme0n1" "/dev/nvme1n1"];
    devices = ["/dev/mapper/encrypted_root" "/dev/mapper/encrypted_root1"];
  };

  imports = [
    ../../profiles/admin-user/home-manager.nix
    ../../profiles/admin-user/user.nix
    ../../profiles/disk/bcachefs-on-luks.nix
    ../../profiles/hardware/ax101.nix
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

  microvm.autostart = [
    "agent-8be5-ac2e"
    "agent-8be5-9792"
    "agent-8be5-c91d"
    "master-8be5-f2ba"
  ];

  programs.ssh.startAgent = true;

  boot.binfmt.emulatedSystems = ["aarch64-linux"];

  system.autoUpgrade = {
    enable = true;
    flake = "github:johnae/world";
    allowReboot = true;
    dates = "06:00";
    randomizedDelaySec = "5min";
    enableSentinel = false; ## not running kubernetes here
  };

  boot.kernelParams = [
    "ip=65.109.92.173::65.109.92.129:255.255.255.192:${hostName}::none"
  ];

  ## for tailscale exit node functionality
  boot.kernel.sysctl."net.ipv4.ip_forward" = 1;

  boot.initrd.availableKernelModules = [
    "igb"
    "nvme"
    "ahci"
    "usbhid"
  ];

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
      "--advertise-tags=tag:server"
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
          "65.109.92.173/26"
          "2a01:4f9:3051:46ed::2/64"
        ];
        routes = [
          {routeConfig.Gateway = "65.109.92.129";}
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
            addressConfig.Address = "10.100.1.1/24";
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
          "prometheus"
          "orion"
          "hyperion"
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
          "prometheus"
          "orion"
          "eris"
        ];
      };
      folders."/home/${adminUser.name}/Photos" = {
        id = "photos";
        devices = [
          "antares"
          "eris"
          "orion"
          "icarus"
          "s23ultra"
        ];

        versioning.type = "staggered";
        versioning.params.cleanInterval = "3600";
        versioning.params.maxAge = "0";
        versioning.params.versionsPath = "/home/${adminUser.name}/Photos/stbackup";
      };
    };
  };

  services.restic.backups.remote.pruneOpts = [
    "--keep-daily 10"
    "--keep-weekly 7"
    "--keep-monthly 12"
    "--keep-yearly 75"
  ];

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
