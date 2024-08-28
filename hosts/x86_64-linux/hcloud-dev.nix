{
  config,
  lib,
  pkgs,
  adminUser,
  hostName,
  ...
}: {
  publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDErC2NyMr7hmuNA9gnuLveTxPjYVqkmpLL9j6kzf2a5";

  imports = [
    ../../profiles/acme.nix
    ../../profiles/admin-user/home-manager.nix
    ../../profiles/admin-user/user.nix
    ../../profiles/hcloud.nix
    ../../profiles/hcloud-remote-unlock.nix
    ../../profiles/home-manager.nix
    ../../profiles/restic-backup.nix
    ../../profiles/state.nix
    ../../profiles/tailscale.nix
    ../../profiles/vaultwarden.nix
    ../../profiles/zram.nix
  ];

  programs.ssh.startAgent = true;

  boot.kernel.sysctl = {
    "fs.inotify.max_user_watches" = 12288;
  };

  system.autoUpgrade = {
    enable = true;
    flake = "github:johnae/world";
    allowReboot = true;
    dates = "06:00";
    randomizedDelaySec = "5min";
  };

  systemd.services.stop-services-before-bootstrapping = {
    description = "Stop services before bootstrapping";
    enable = true;
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = "yes";
    };
    script = ''
      if [ ! -e /run/stop-services-before-bootstrapping ]; then
        touch /run/stop-services-before-bootstrapping
        systemctl stop acme-bw.9000.dev.timer || true
        systemctl stop acme-bw.9000.dev.service || true
        systemctl stop restic-backups-remote.timer || true
        systemctl stop vaultwarden || true
      else
        echo "skipping stop services before bootstrapping - /run/stop-services-before-bootstrapping exists"
      fi
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
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = "yes";
      EnvironmentFile = [
        config.services.restic.backups.remote.environmentFile
        config.age.secrets.cloudflare-env.path
      ];
    };
    script = ''
      if [ ! -e /run/bootstrapped ]; then
        touch /run/bootstrapped
        systemctl stop acme-bw.9000.dev.timer || true
        systemctl stop acme-bw.9000.dev.service || true
        systemctl stop restic-backups-remote.timer || true
        systemctl stop vaultwarden || true
        mkdir -p /root/.cache
        rm -rf /var/lib/vaultwarden/*

        ${pkgs.restic}/bin/restic restore latest:/home/john/Development --target /home/john/Development --host ${hostName} || true
        chown -R ${toString adminUser.uid}:${toString adminUser.gid} /home/john/Development

        ${pkgs.restic}/bin/restic restore latest:/var/lib/vw-backup --target /var/lib/vw-backup --host ${hostName} || true
        ${pkgs.restic}/bin/restic restore latest:/var/lib/acme --target /var/lib/acme --host ${hostName} || true

        systemctl start restic-backups-remote.timer
        systemctl start acme-bw.9000.dev.timer
        systemctl restart vaultwarden

        RECORD_ID="$(${pkgs.flarectl}/bin/flarectl --json dns list --zone 9000.dev | ${pkgs.jq}/bin/jq -r '.[] | select(.Name == "bw.9000.dev") | .ID')"
        TS_IP="$(${pkgs.tailscale}/bin/tailscale status --json | ${pkgs.jq}/bin/jq -r '.Self.TailscaleIPs[0]')"
        echo "Map bw.9000.dev ($RECORD_ID) to $TS_IP"
        ${pkgs.flarectl}/bin/flarectl --json dns update --zone 9000.dev --id "$RECORD_ID" --type A --ttl 60 --content "$TS_IP"
      else
        echo "skipping bootstrap service - /run/bootstrapped exists"
      fi
    '';
    after = ["network-online.target" "tailscale-auth.service" "stop-services-before-bootstrapping.service"];
    requires = ["network-online.target" "tailscale-auth.service" "stop-services-before-bootstrapping.service"];
    wantedBy = ["multi-user.target"];
  };

  age.secrets = {
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
    cloudflare-env.file = ../../secrets/cloudflare-env.age;
    vaultwarden-env.file = ../../secrets/vaultwarden-env.age;
  };

  networking.firewall.trustedInterfaces = ["tailscale0"];

  security.sudo.wheelNeedsPassword = false;

  services.tailscale.auth = {
    enable = true;
    after = ["metadata.service"];
    args.ssh = true;
    args.accept-routes = false;
    args.accept-dns = true;
    args.advertise-exit-node = true;
    args.auth-key = "file:/etc/ts-auth-key";
    args.hostname = "\"$NODENAME\"";
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

  services.restic.backups.remote.pruneOpts = [
    "--keep-daily 10"
    "--keep-weekly 7"
    "--keep-monthly 12"
    "--keep-yearly 75"
  ];

  services.restic.backups.remote.paths = [
    "/var/lib/vw-backup"
    "/var/lib/acme"
  ];

  boot.initrd.network.ssh.authorizedKeys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ266FsDSw6v4gU9PwSun1aLKpS/BML4QOB1Cii9y1dM" ## gh
  ];

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIzm5RyD+1nfy1LquvkEog4SZtPgdhzjr49jSC8PAinp"
  ];

  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
    users.${adminUser.name} = {
      imports = [../../users/profiles/headless.nix];
      programs.git.extraConfig.user.signingKey = config.age.secrets.id_ed25519_alt.path;
    };
  };

  disko.devices = {
    disk.disk1 = {
      device = lib.mkDefault "/dev/sda";
      type = "disk";
      content = {
        type = "gpt";
        partitions = {
          boot = {
            name = "boot";
            size = "1M";
            type = "EF02";
          };
          esp = {
            name = "ESP";
            size = "500M";
            type = "EF00";
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot";
            };
          };
          luks = {
            size = "100%";
            content = {
              type = "luks";
              name = "encrypted";
              settings.allowDiscards = true;
              passwordFile = "/tmp/disk.key";
              content = {
                type = "filesystem";
                format = "bcachefs";
                mountpoint = "/";
                mountOptions = ["defaults" "compression=zstd" "background_compression=zstd"];
              };
            };
          };
        };
      };
    };
  };
}
