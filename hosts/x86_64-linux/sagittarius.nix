{
  adminUser,
  config,
  hostName,
  pkgs,
  lib,
  ...
}: {
  publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIOWdmZbf9/MQKKJEvb8LlOI/xeuAMt+GhNQjlKYzTCI";

  bcachefs = {
    disks = ["/dev/nvme0n1"];
    devices = ["/dev/mapper/encrypted_root"];
  };

  ## always fsck and fix errors
  fileSystems."/keep".options = lib.mkForce ["defaults" "compression=zstd" "background_compression=zstd" "fsck" "fix_errors"];

  imports = [
    ../../profiles/admin-user/home-manager.nix
    ../../profiles/admin-user/user.nix
    ../../profiles/disk/bcachefs-on-luks.nix
    ../../profiles/hardware/tlsense.nix
    ../../profiles/core-metrics.nix
    ../../profiles/core-logging.nix
    ../../profiles/home-manager.nix
    ../../profiles/server.nix
    ../../profiles/state.nix
    ../../profiles/tailscale.nix
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

  boot.initrd = {
    systemd.enable = true;
    systemd.tpm2.enable = true;
  };

  services.tailscale.auth = {
    enable = true;
    args.advertise-tags = ["tag:server"];
    args.ssh = true;
    args.accept-routes = false;
    args.accept-dns = false;
    args.advertise-exit-node = true;
    args.auth-key = "file:${config.age.secrets.ts-google-9k.path}";
  };

  services.jae.router = {
    enable = true;
    useNextDns = true;
    nextDnsEnvFile = "/var/run/agenix/nextdns";
    restrictedMacs = [
      "5c:e0:c5:8a:24:6a"
      "b4:18:d1:ab:4e:5a"
    ];
    upstreamDnsServers = [
      "2a07:a8c1::"
      "45.90.30.0"
      "2a07:a8c0::"
      "45.90.28.0"
    ];
    externalInterface = "enp1s0f0";
    internalInterface = "enp2s0";
    internalInterfaceIP = "192.168.20.1";
    dnsMasqSettings.no-resolv = true;
    dnsMasqSettings.bogus-priv = true;
    dnsMasqSettings.strict-order = true;
  };

  services.prometheus.exporters = {
    dnsmasq = {
      enable = true;
      dnsmasqListenAddress = "localhost:5342";
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
          job_name = "dnsmasq";
          scrape_interval = "10s";
          static_configs = [
            {targets = ["127.0.0.1:9153"];}
          ];
          inherit relabel_configs;
        }
        {
          job_name = "corerad";
          scrape_interval = "10s";
          static_configs = [
            {targets = ["127.0.0.1:9430"];}
          ];
          inherit relabel_configs;
        }
      ];
    };
  };

  age.secrets = {
    ts-google-9k = {
      file = ../../secrets/ts-google-9k.age;
      owner = "1337";
    };
    nextdns = {
      file = ../../secrets/nextdns.age;
    };
  };

  users.users.${adminUser.name}.shell = lib.mkForce pkgs.bashInteractive;

  home-manager = {
    users.${adminUser.name} = {
      imports = [../../users/profiles/minimal.nix];
    };
  };
}
