{
  adminUser,
  hostName,
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
    "ip=65.109.85.161::65.109.85.129:255.255.255.192:${hostName}:eth0:none"
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
      ## This isn't so nice. Have to copy the file to /keep/secrets and keep it there.
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

  services.tailscale.auth = {
    enable = true;
    args.advertise-tags = ["tag:server"];
    args.ssh = true;
    args.accept-routes = false;
    args.accept-dns = false;
    args.advertise-exit-node = true;
    args.auth-key = "file:/var/run/agenix/ts-google-9k";
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
        locations."/".proxyPass = "http://localhost:8222";
        locations."/".proxyWebsockets = true;
      };
      "bw.ill.dev" = {
        useACMEHost = "bw.ill.dev";
        forceSSL = true;
        locations."/".proxyPass = "http://localhost:8222";
        locations."/".proxyWebsockets = true;
      };
    };
  };

  services.my-cloudflared = {
    enable = true;
    tunnels."9000-tunnel" = {
      credentialsFile = "/run/agenix/cloudflare-tunnel-9k";
      default = "http://localhost";
      originRequest.noTLSVerify = true;
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

  networking = {
    defaultGateway = "65.109.85.129";
    defaultGateway6 = {
      address = "fe80::1";
      interface = "eth0";
    };
    firewall.trustedInterfaces = ["tailscale0"];
    interfaces.eth0.ipv4.addresses = [
      {
        address = "65.109.85.161";
        prefixLength = 26;
      }
    ];

    interfaces.eth0.ipv6.addresses = [
      {
        address = "2a01:4f9:3051:5389::2";
        prefixLength = 64;
      }
    ];
  };

  age.secrets = {
    initrd-key = {
      file = ../../secrets/${hostName}/initrd_ed25519_key.age;
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
      owner = "cloudflared";
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

      folders."/home/${adminUser.name}/Sync" = {
        id = "sync";
        devices = [
          "antares"
          "eris"
          "hyperion"
          "polaris"
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
          "polaris"
          "eris"
        ];
      };
      folders."/home/${adminUser.name}/Photos" = {
        id = "photos";
        devices = [
          "antares"
          "eris"
          "polaris"
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
