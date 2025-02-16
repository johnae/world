{
  adminUser,
  config,
  lib,
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

  ## always fsck and fix errors
  fileSystems."/keep".options = lib.mkForce ["defaults" "compression=zstd" "background_compression=zstd" "fsck" "fix_errors"];

  boot.initrd.luks.devices.cryptkey.keyFile = "/dev/disk/by-partlabel/alt_cryptkey";

  imports = [
    ../../profiles/acme.nix
    ../../profiles/admin-user/home-manager.nix
    ../../profiles/admin-user/user.nix
    ../../profiles/disk/bcachefs-on-luks.nix
    ../../profiles/hardware/b550.nix
    ../../profiles/core-metrics.nix
    ../../profiles/core-logging.nix
    ../../profiles/forgejo.nix
    ../../profiles/home-manager.nix
    ../../profiles/restic-backup.nix
    ../../profiles/server.nix
    ../../profiles/state.nix
    ../../profiles/syncthing.nix
    ../../profiles/tailscale.nix
    ../../profiles/vaultwarden.nix
    ../../profiles/zram.nix
  ];

  virtualisation.docker.enable = false;
  virtualisation.podman.enable = true;
  virtualisation.podman.dockerCompat = true;

  system.autoUpgrade = {
    enable = true;
    flake = "github:johnae/world";
    allowReboot = true;
    dates = "06:00";
    randomizedDelaySec = "5min";
    enableSentinel = false; ## not running kubernetes here
  };

  boot.kernel = {
    ## for tailscale exit node functionality
    sysctl."net.ipv4.ip_forward" = 1;

    # Reboot this many seconds after panic
    sysctl."kernel.panic" = 20;

    # Panic if the kernel detects an I/O channel
    # check (IOCHK). 0=no | 1=yes
    sysctl."kernel.panic_on_io_nmi" = 1;

    # Panic if a hung task was found. 0=no, 1=yes
    sysctl."kernel.hung_task_panic" = 1;

    # Setup timeout for hung task,
    # in seconds (suggested 300)
    sysctl."kernel.hung_task_timeout_secs" = 300;

    # Panic on out of memory.
    # 0=no | 1=usually | 2=always
    sysctl."vm.panic_on_oom" = 1;

    # Panic when the kernel detects an NMI
    # that usually indicates an uncorrectable
    # parity or ECC memory error. 0=no | 1=yes
    sysctl."kernel.panic_on_unrecovered_nmi" = 1;
  };

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

  services.buildkite-nix-builder = {
    enable = true;
    runtimePackages = [
      pkgs.bash
      pkgs.cachix
      pkgs.coreutils
      pkgs.curl
      pkgs.git
      pkgs.gnutar
      pkgs.gzip
      pkgs.jq
      pkgs.nix
      pkgs.openssl
      pkgs.procps
    ];
    tags = {
      nix = "true";
      nixos = "true";
      linux = "true";
      arch = "x86_64-linux";
      queue = "default-queue";
    };
  };

  services.tailscale.auth = {
    enable = true;
    args.advertise-tags = ["tag:server"];
    args.ssh = true;
    args.accept-routes = false;
    args.accept-dns = false;
    args.advertise-exit-node = true;
    args.auth-key = config.age.secrets.ts-google-9k.path;
  };

  # microvm.autostart = [
  #   "agent-8be5-ac2e"
  #   "agent-8be5-9792"
  #   "agent-8be5-c91d"
  #   "master-8be5-f2ba"
  # ];

  # microvm = let
  #   vms = [
  #     "playground"
  #   ];
  # in {
  #   vms = builtins.listToAttrs (map (name: {
  #       inherit name;
  #       value = {
  #         flake = self;
  #         updateFlake = "github:johnae/world";
  #       };
  #     })
  #     vms);
  #   autostart = vms;
  # };

  networking.useDHCP = false;
  # networking.nat = {
  #   enable = true;
  #   enableIPv6 = true;
  #   internalInterfaces = ["microvm"];
  # };
  # networking.firewall.trustedInterfaces = ["tailscale0" "microvm"];
  networking.firewall.trustedInterfaces = ["tailscale0"];

  systemd.network = {
    enable = true;
    # netdevs = {
    #   "10-microvm".netdevConfig = {
    #     Kind = "bridge";
    #     Name = "microvm";
    #   };
    # };
    networks = {
      "10-wan" = {
        ## udevadm test-builtin net_id /sys/class/net/eth0
        ## https://www.freedesktop.org/software/systemd/man/latest/systemd.net-naming-scheme.html
        matchConfig.Name = ["enp*"];
        address = [
          "192.168.20.143/24"
        ];
        routes = [
          {Gateway = "192.168.20.1";}
        ];
        linkConfig.RequiredForOnline = "routable";
      };
      # "10-microvm" = {
      #   matchConfig.Name = "microvm";
      #   networkConfig = {
      #     DHCPServer = true;
      #     IPv6SendRA = true;
      #   };
      #   addresses = [
      #     {
      #       Address = "10.100.1.1/24";
      #     }
      #   ];
      # };
      # "11-microvm" = {
      #   matchConfig.Name = "vm-*";
      #   networkConfig.Bridge = "microvm";
      # };
    };
  };

  systemd.services.stop-services-before-bootstrapping = {
    description = "Stop services before bootstrapping";
    enable = true;
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = "yes";
    };
    unitConfig.ConditionPathExists = "!/keep/stop-services-before-bootstrapping";
    script = ''
      touch /keep/stop-services-before-bootstrapping
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
    unitConfig.ConditionPathExists = "!/keep/bootstrapped";
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = "yes";
      EnvironmentFile = [
        config.services.restic.backups.remote.environmentFile
        config.age.secrets.cloudflare-env.path
      ];
    };
    script = ''
      touch /keep/bootstrapped
      systemctl stop acme-bw.9000.dev.timer || true
      systemctl stop acme-bw.9000.dev.service || true
      systemctl stop restic-backups-remote.timer || true
      systemctl stop vaultwarden || true
      mkdir -p /root/.cache
      rm -rf /var/lib/vaultwarden/*
      rm -rf /var/lib/bitwarden_rs/*

      ${pkgs.restic}/bin/restic restore latest:/var/lib/vw-backup --target /var/lib/vw-backup --host ${hostName} || true
      ${pkgs.restic}/bin/restic restore latest:/var/lib/acme --target /var/lib/acme --host ${hostName} || true
      chown acme:acme /var/lib/acme
      chown -R acme:nginx /var/lib/acme/*

      systemctl start restic-backups-remote.timer
      systemctl start acme-bw.9000.dev.timer
      systemctl restart vaultwarden
      systemctl restart nginx
    '';
    after = ["network-online.target" "stop-services-before-bootstrapping.service"];
    requires = ["network-online.target" "stop-services-before-bootstrapping.service"];
    wantedBy = ["multi-user.target"];
  };

  age.secrets = {
    cloudflare-env.file = ../../secrets/cloudflare-env.age;
    cloudflare-tunnel-9k = {
      file = ../../secrets/cloudflare-tunnel-9k.age;
      owner = "cloudflared";
    };
    cloudflare-tunnel-cert-9k = {
      file = ../../secrets/cloudflare-tunnel-cert-9k.age;
      owner = "cloudflared";
    };
    conduwuit-registration-token = {
      file = ../../secrets/conduwuit-registration-token.age;
      mode = "777";
    };
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
    ssh_host_microvm_ed25519_key = {
      file = ../../secrets/ssh_host_microvm_ed25519_key.age;
      path = "/var/lib/microvm-secrets/ssh_host_ed25519_key";
      symlink = false;
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

  security.acme.certs = {
    "bw.9000.dev" = {
      group = "nginx";
    };
    "chat.9000.dev" = {
      group = "nginx";
    };
    "tika.9000.dev" = {
      group = "nginx";
    };
    "grafana.9000.dev" = {
      group = "nginx";
    };
    "victoriametrics.9000.dev" = {
      group = "nginx";
    };
    "victorialogs.9000.dev" = {
      group = "nginx";
    };
  };

  services.my-cloudflared = {
    enable = true;
    tunnels."9000-tunnel" = {
      credentialsFile = config.age.secrets.cloudflare-tunnel-9k.path;
      default = "http://localhost";
      originRequest.noTLSVerify = true;
      originCertPath = config.age.secrets.cloudflare-tunnel-cert-9k.path;
    };
  };

  services.my-matrix = {
    enable = true;
    server_name = "9000.dev";
  };

  services.cloudflare-tailscale-dns.ollama = {
    enable = true;
    zone = "9000.dev";
    cloudflareEnvFile = config.age.secrets.cloudflare-env.path;
    host = "eris";
  };

  services.cloudflare-tailscale-dns.bw = {
    enable = true;
    zone = "9000.dev";
    cloudflareEnvFile = config.age.secrets.cloudflare-env.path;
  };

  services.cloudflare-tailscale-dns.tika = {
    enable = true;
    zone = "9000.dev";
    cloudflareEnvFile = config.age.secrets.cloudflare-env.path;
  };

  services.cloudflare-tailscale-dns.chat = {
    enable = true;
    zone = "9000.dev";
    cloudflareEnvFile = config.age.secrets.cloudflare-env.path;
  };

  services.cloudflare-tailscale-dns.grafana = {
    enable = true;
    zone = "9000.dev";
    cloudflareEnvFile = config.age.secrets.cloudflare-env.path;
  };

  services.cloudflare-tailscale-dns.victoriametrics = {
    enable = true;
    zone = "9000.dev";
    cloudflareEnvFile = config.age.secrets.cloudflare-env.path;
  };

  services.cloudflare-tailscale-dns.victorialogs = {
    enable = true;
    zone = "9000.dev";
    cloudflareEnvFile = config.age.secrets.cloudflare-env.path;
  };

  services.nginx.tailscaleAuth = {
    enable = true;
    virtualHosts = [
      "chat.9000.dev"
      "grafana.9000.dev"
    ];
  };

  services.tika.enable = true;

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

  services.open-webui.enable = false;
  services.open-webui.port = 11112;
  services.open-webui.environment = {
    # PYDANTIC_SKIP_VALIDATING_CORE_SCHEMAS = "True";
    OLLAMA_BASE_URL = "http://eris:11434";
    ENABLE_OLLAMA_API = "true";
    DEFAULT_USER_ROLE = "user";
    WEBUI_AUTH = "true";
    WEBUI_AUTH_TRUSTED_EMAIL_HEADER = "X-Webauth-Email";
    WEBUI_AUTH_TRUSTED_NAME_HEADER = "X-Webauth-Name";
    ENABLE_OAUTH_SIGNUP = "true";
    ENABLE_SIGNUP = "true";
    WEBUI_URL = "https://chat.9000.dev";
    OAUTH_MERGE_ACCOUNTS_BY_EMAIL = "true";
  };

  environment.persistence."/keep".directories = [
    "/var/lib/private/open-webui"
    "/var/lib/private/victoriametrics"
    "/var/lib/private/victorialogs"
  ];

  age.secrets = {
    "buildkite-agent-exporter-token" = {
      file = ../../secrets/buildkite-token.age;
      owner = config.services.prometheus.exporters.buildkite-agent.user;
    };
  };

  services.prometheus.exporters = {
    buildkite-agent = {
      enable = true;
      tokenPath = config.age.secrets.buildkite-agent-exporter-token.path;
    };
  };

  services.vmagent = {
    prometheusConfig = let
      relabel_configs = [
        {
          action = "replace";
          replacement = hostName;
          target_label = "instance";
        }
      ];
    in {
      scrape_configs = [
        {
          job_name = "buildkite-agent";
          scrape_interval = "10s";
          static_configs = [
            {targets = ["127.0.0.1:9876"];}
          ];
          inherit relabel_configs;
        }
      ];
    };
  };

  services.grafana.enable = true;
  services.grafana.declarativePlugins = [pkgs.victoriametrics-logs-datasource-plugin pkgs.victoriametrics-metrics-datasource-plugin];
  services.grafana.provision.enable = true;
  services.grafana.provision.datasources.settings = {
    datasources = [
      {
        name = "VictoriaLogs";
        type = "victoriametrics-logs-datasource";
        access = "proxy";
        url = "https://victorialogs.9000.dev";
      }
      {
        name = "VictoriaMetrics";
        type = "victoriametrics-metrics-datasource";
        access = "proxy";
        url = "https://victoriametrics.9000.dev";
        isDefault = true;
      }
      {
        name = "Prometheus";
        type = "prometheus";
        access = "proxy";
        url = "https://victoriametrics.9000.dev";
      }
    ];
  };
  services.grafana.settings = {
    server = {
      enable_gzip = true;
      http_port = 3000;
      http_addr = "127.0.0.1";
    };
    "auth.proxy" = {
      enabled = true;
      header_name = "X-WebAuth-Email";
      header_property = "username";
      auto_sign_up = true;
      sync_ttl = 60;
      whitelist = "127.0.0.1";
      headers = "Name:X-WebAuth-Name Email:X-WebAuth-Email";
      enable_login_token = true;
    };
    plugins = {
      allow_loading_unsigned_plugins = "victoriametrics-metrics-datasource,victoriametrics-logs-datasource";
    };
  };

  services.victoriametrics.enable = true;
  services.victorialogs.enable = true;

  services.nginx = {
    enable = true;
    recommendedTlsSettings = true;
    recommendedProxySettings = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    clientMaxBodySize = "300m";
    commonHttpConfig = ''
      map $auth_user $auth_email {
        ~^(?<user_no_dash>[^-]+)-.*$ $user_no_dash@9000.dev;
        ~^(?<user>[^@]+)@passkey$ $user@9000.dev;
        default $auth_user;
      }
    '';
    virtualHosts = {
      "tika.9000.dev" = {
        useACMEHost = "tika.9000.dev";
        locations."/".proxyPass = "http://localhost:9998";
        locations."/".proxyWebsockets = true;
        forceSSL = true;
      };
      "bw.9000.dev" = {
        useACMEHost = "bw.9000.dev";
        locations."/".proxyPass = "http://localhost:8222";
        locations."/".proxyWebsockets = true;
        forceSSL = true;
      };
      "chat.9000.dev" = {
        useACMEHost = "chat.9000.dev";
        locations."/".proxyPass = "http://localhost:11112";
        locations."/".proxyWebsockets = true;
        locations."/".extraConfig = ''
          proxy_set_header X-Webauth-Email "$auth_email";
        '';
        forceSSL = true;
      };
      "grafana.9000.dev" = {
        useACMEHost = "grafana.9000.dev";
        locations."/".proxyPass = "http://localhost:3000";
        locations."/".proxyWebsockets = true;
        locations."/".extraConfig = ''
          proxy_set_header X-Webauth-Email "$auth_email";
        '';
        forceSSL = true;
      };
      "victoriametrics.9000.dev" = {
        useACMEHost = "victoriametrics.9000.dev";
        locations."/".proxyPass = "http://localhost:8428";
        locations."/".proxyWebsockets = true;
        forceSSL = true;
      };
      "victorialogs.9000.dev" = {
        useACMEHost = "victorialogs.9000.dev";
        locations."/".proxyPass = "http://localhost:9428";
        locations."/".proxyWebsockets = true;
        forceSSL = true;
      };
    };
  };

  services.restic = {
    backups = {
      remote = {
        backupPrepareCommand = "${pkgs.restic}/bin/restic unlock";
        pruneOpts = [
          "--keep-daily 10"
          "--keep-weekly 7"
          "--keep-monthly 12"
          "--keep-yearly 75"
        ];
        paths = [
          "/var/lib/vw-backup"
          "/var/lib/matrix-conduit/backups"
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
          "eris"
          "hyperion"
          "sirius"
          "s8plus"
          "titan"
          "cygnus"
          "z6fold"
        ];
      };
      folders."/home/${adminUser.name}/Photos" = {
        id = "photos";
        devices = [
          "antares"
          "eris"
          "cygnus"
          "sirius"
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
