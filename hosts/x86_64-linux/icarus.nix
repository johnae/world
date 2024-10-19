{
  adminUser,
  config,
  pkgs,
  hostName,
  ...
}: {
  publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHaa82NwBC+ty4Wyeuf5kdava7huSYF6k0NYF2ahwayW";
  syncthingDeviceID = "HBL5ZRB-R2STGW5-LMAYYHX-KOFTP3X-VO4IV6E-PEDKZ3N-WCRR7BY-F5C7AAP";

  bcachefs = {
    disks = ["/dev/nvme0n1" "/dev/nvme1n1"];
    devices = ["/dev/mapper/encrypted_root" "/dev/mapper/encrypted_root1"];
  };

  imports = [
    ../../profiles/acme.nix
    ../../profiles/admin-user/home-manager.nix
    ../../profiles/admin-user/user.nix
    ../../profiles/disk/bcachefs-on-luks.nix
    ../../profiles/hardware/b550.nix
    ../../profiles/home-manager.nix
    ../../profiles/server.nix
    ../../profiles/restic-backup.nix
    ../../profiles/state.nix
    ../../profiles/syncthing.nix
    ../../profiles/tailscale.nix
    ../../profiles/vaultwarden.nix
    ../../profiles/zram.nix
  ];

  system.autoUpgrade = {
    enable = true;
    flake = "github:johnae/world";
    allowReboot = true;
    dates = "06:00";
    randomizedDelaySec = "5min";
    enableSentinel = false; ## not running kubernetes here
  };

  ## for tailscale exit node functionality
  boot.kernel.sysctl."net.ipv4.ip_forward" = 1;

  boot.kernelParams = [
    "ip=192.168.20.143::192.168.20.1:255.255.255.0:${hostName}:eth0:none"
  ];

  boot.initrd.availableKernelModules = [
    "igc"
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

  networking = {
    defaultGateway = "192.168.20.1";
    firewall.trustedInterfaces = ["tailscale0"];
    interfaces.eth0.ipv4.addresses = [
      {
        address = "192.168.20.143";
        prefixLength = 24;
      }
    ];
  };

  systemd.services.stop-services-before-bootstrapping = {
    description = "Stop services before bootstrapping";
    enable = true;
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = "yes";
    };
    unitConfig.ConditionPathExists = "!/run/stop-services-before-bootstrapping";
    script = ''
      touch /run/stop-services-before-bootstrapping
      systemctl stop acme-bw.9000.dev.timer || true
      systemctl stop acme-bw.9000.dev.service || true
      systemctl stop restic-backups-remote.timer || true
      systemctl stop vaultwarden || true
    '';
    before = ["acme-bw.9000.dev.timer" "acme-bw.9000.dev.service" "restic-backups-remote.timer" "vaultwarden.service" "bootstrap.service"];
    wantedBy = ["multi-user.target"];
  };

  systemd.services.bootstrap = {
    description = "Bootstrap machine on first boot";
    environment = {
      RESTIC_PASSWORD_FILE = config.services.restic.backups.remote.passwordFile;
      RESTIC_REPOSITORY = config.services.restic.backups.remote.repository;
      XDG_CACHE_HOME = "/root/.cache";
      HOME = "/root";
    };
    enable = true;
    unitConfig.ConditionPathExists = "!/run/bootstrapped";
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = "yes";
      EnvironmentFile = [
        config.services.restic.backups.remote.environmentFile
        config.age.secrets.cloudflare-env.path
      ];
    };
    script = ''
      touch /run/bootstrapped
      systemctl stop acme-bw.9000.dev.timer || true
      systemctl stop acme-bw.9000.dev.service || true
      systemctl stop restic-backups-remote.timer || true
      systemctl stop vaultwarden || true
      mkdir -p /root/.cache
      rm -rf /var/lib/vaultwarden/*

      ${pkgs.restic}/bin/restic restore latest:/var/lib/vw-backup --target /var/lib/vw-backup --host ${hostName} || true
      ${pkgs.restic}/bin/restic restore latest:/var/lib/acme --target /var/lib/acme --host ${hostName} || true
      chown acme:acme /var/lib/acme
      chown -R acme:nginx /var/lib/acme/*

      systemctl start restic-backups-remote.timer
      systemctl start acme-bw.9000.dev.timer
      systemctl restart vaultwarden
      systemctl restart nginx

      RECORD_ID="$(${pkgs.flarectl}/bin/flarectl --json dns list --zone 9000.dev | ${pkgs.jq}/bin/jq -r '.[] | select(.Name == "bw.9000.dev") | .ID')"
      TS_IP="$(${pkgs.tailscale}/bin/tailscale status --json | ${pkgs.jq}/bin/jq -r '.Self.TailscaleIPs[0]')"
      echo "Map bw.9000.dev ($RECORD_ID) to $TS_IP"
      ${pkgs.flarectl}/bin/flarectl --json dns update --zone 9000.dev --id "$RECORD_ID" --type A --ttl 60 --content "$TS_IP"
    '';
    after = ["network-online.target" "tailscale-auth.service" "stop-services-before-bootstrapping.service"];
    requires = ["network-online.target" "tailscale-auth.service" "stop-services-before-bootstrapping.service"];
    wantedBy = ["multi-user.target"];
  };

  age.secrets = {
    cloudflare-env.file = ../../secrets/cloudflare-env.age;
    vaultwarden-env.file = ../../secrets/vaultwarden-env.age;
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

  security.acme.certs = {
    "bw.9000.dev" = {
      group = "nginx";
    };
  };

  services.vaultwarden = {
    enable = true;
    environmentFile = config.age.secrets.vaultwarden-env.path;
    backupDir = "/var/lib/vw-backup";

    config = {
      DOMAIN = "https://bw.9000.dev";
      SIGNUPS_ALLOWED = "false";
      PASSWORD_HINTS_ALLOWED = "false";
      ROCKET_ADDRESS = "127.0.0.1";
      ROCKET_PORT = 8222;
      PASSWORD_ITERATIONS = 600000;
    };
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

  services.restic = {
    backups = {
      remote = {
        pruneOpts = [
          "--keep-daily 10"
          "--keep-weekly 7"
          "--keep-monthly 12"
          "--keep-yearly 75"
        ];
        paths = [
          "/var/lib/vw-backup"
          "/var/lib/acme"
        ];
      };
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
          "sirius"
          "s23ultra"
          "s8plus"
          "titan"
        ];
      };
      folders."/home/${adminUser.name}/Photos" = {
        id = "photos";
        devices = [
          "antares"
          "eris"
          "sirius"
          "s23ultra"
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
      imports = [../../users/profiles/headless.nix];
    };
  };
}
