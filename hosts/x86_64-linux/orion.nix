{
  adminUser,
  hostName,
  pkgs,
  config,
  ...
}: {
  publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEEPD945cTDxeNhGljSKqQfRCUeXcwIDKOBD847OECQs";
  syncthingDeviceID = "XOPUBYF-LTOERSA-NGLA6ZJ-BU455JS-JOTCFQP-JGZP6WC-VRUTNOX-5YEUVQD";

  bcachefs = {
    disks = ["/dev/nvme0n1" "/dev/nvme1n1"];
    devices = ["/dev/mapper/encrypted_root" "/dev/mapper/encrypted_root1"];
  };

  imports = [
    ../../profiles/acme.nix
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
    ../../profiles/vaultwarden.nix
    ../../profiles/zram.nix
  ];

  virtualisation = {
    docker.enable = false;
    podman.enable = true;
    podman.dockerCompat = true;
    libvirtd.enable = true;
  };

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
    "ip=65.109.85.161::65.109.85.129:255.255.255.192:${hostName}::none"
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
      "--advertise-tags=tag:server"
      "--ssh"
    ];
  };

  services.nginx = {
    enable = true;
    recommendedTlsSettings = true;
    recommendedProxySettings = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    clientMaxBodySize = "300m";
    virtualHosts = {
      "bw.9000.dev" = {
        useACMEHost = "bw.9000.dev";
        locations."/".proxyPass = "http://localhost:8222";
        locations."/".proxyWebsockets = true;
        forceSSL = true;
      };
    };
  };

  # services.my-cloudflared = {
  #   enable = true;
  #   tunnels."9000-tunnel" = {
  #     credentialsFile = "/run/agenix/cloudflare-tunnel-9k";
  #     default = "http://localhost";
  #     originRequest.noTLSVerify = true;
  #   };
  # };

  services.redis = {
    package = pkgs.valkey;
    servers = {
      default = {
        enable = true;
        appendOnly = true;
        bind = null;
        port = 6379;
        settings = {
          protected-mode = false;
        };
      };
    };
  };

  security.acme.certs = {
    "bw.ill.dev" = {
      group = "nginx";
    };
    "bw.9000.dev" = {
      group = "nginx";
    };
    "bw.johnae.dev" = {
      group = "nginx";
    };
  };

  environment.persistence."/keep" = {
    directories = [
      "/var/lib/acme"
      "/var/lib/redis-default"
    ];
  };

  services.vaultwarden = {
    enable = true;
    environmentFile = "/run/agenix/vaultwarden-env";
    backupDir = "/var/lib/vaultwarden-backup";

    config = {
      DOMAIN = "https://bw.9000.dev";
      SIGNUPS_ALLOWED = "false";
      PASSWORD_HINTS_ALLOWED = "false";
      ROCKET_ADDRESS = "127.0.0.1";
      ROCKET_PORT = 8222;
      PASSWORD_ITERATIONS = 600000;
    };
  };

  networking.useDHCP = false;
  networking.nat = {
    enable = true;
    enableIPv6 = true;
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
    networks = {
      "10-wan" = {
        ## udevadm test-builtin net_id /sys/class/net/eth0
        ## https://www.freedesktop.org/software/systemd/man/latest/systemd.net-naming-scheme.html
        matchConfig.Name = ["enp*"];
        address = [
          "65.109.85.161/26"
          "2a01:4f9:3051:5389::2/64"
        ];
        routes = [
          {routeConfig.Gateway = "65.109.85.129";}
          {routeConfig.Gateway = "fe80::1";}
        ];
        linkConfig.RequiredForOnline = "routable";
      };
    };
  };

  networking.firewall.trustedInterfaces = ["tailscale0"];

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

    cloudflare-tunnel-9k = {
      file = ../../secrets/cloudflare-tunnel-9k.age;
      # owner = "cloudflared";
    };

    cloudflare-env.file = ../../secrets/cloudflare-env.age;
    vaultwarden-env.file = ../../secrets/vaultwarden-env.age;
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
          "antares"
          "eris"
          "hyperion"
          "cygnus"
          "prometheus"
          "sirius"
          "icarus"
          "s23ultra"
          "s8plus"
          "z6fold"
          "titan"
        ];
      };
      folders."/home/${adminUser.name}/Pictures" = {
        id = "pictures";
        devices = [
          "antares"
          "prometheus"
          "cygnus"
          "sirius"
          "eris"
        ];
      };
      folders."/home/${adminUser.name}/Photos" = {
        id = "photos";
        devices = [
          "antares"
          "eris"
          "cygnus"
          "sirius"
          "icarus"
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

  services.restic.backups.remote.pruneOpts = [
    "--keep-daily 10"
    "--keep-weekly 7"
    "--keep-monthly 12"
    "--keep-yearly 75"
  ];

  home-manager = {
    users.${adminUser.name} = {
      imports = [../../users/profiles/headless.nix];
      programs.git.extraConfig.user.signingKey = config.age.secrets.id_ed25519_alt.path;
    };
  };
}
