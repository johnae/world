{
  adminUser,
  config,
  hostName,
  pkgs,
  lib,
  ...
}:
let
  # Detect public IPv4 from the WAN interface
  getPublicIpv4 = pkgs.writeShellScript "get-public-ipv4" ''
    ${pkgs.iproute2}/bin/ip -4 -json addr show dev enp1s0f0 scope global | \
      ${pkgs.jq}/bin/jq -r '.[0].addr_info[0].local // empty'
  '';

  # Generate Jool NAT64 config with detected IP
  generateJoolConfig = pkgs.writeShellScript "generate-jool-config" ''
    PUBLIC_IP=$(${getPublicIpv4})

    if [ -z "$PUBLIC_IP" ]; then
      echo "ERROR: Could not detect public IPv4 address on enp1s0f0" >&2
      exit 1
    fi

    echo "Configuring Jool NAT64 with public IP: $PUBLIC_IP" >&2

    # Generate the JSON config with the detected IP
    cat > /run/jool-nat64-default.conf <<EOF
    {
      "instance": "default",
      "framework": "netfilter",
      "global": {
        "pool6": "64:ff9b::/96",
        "manually-enabled": true
      },
      "pool4": [
        {
          "protocol": "TCP",
          "prefix": "$PUBLIC_IP/32",
          "port range": "10000-65535"
        },
        {
          "protocol": "UDP",
          "prefix": "$PUBLIC_IP/32",
          "port range": "10000-65535"
        },
        {
          "protocol": "ICMP",
          "prefix": "$PUBLIC_IP/32",
          "port range": "10000-65535"
        }
      ]
    }
    EOF
  '';
in
{
  publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINssAv/UibH5i9JxHFFWNNodKzmjYvPxx4mhTys3S1ZX";

  bcachefs = {
    disks = ["/dev/nvme0n1"];
    devices = ["/dev/mapper/encrypted_root"];
  };

  imports = [
    ../../../profiles/admin-user/home-manager.nix
    ../../../profiles/admin-user/user.nix
    ../../../profiles/disk/bcachefs-on-luks.nix
    ../../../profiles/hardware/tlsense.nix
    ../../../profiles/core-metrics.nix
    ../../../profiles/core-logging.nix
    ../../../profiles/home-manager.nix
    ../../../profiles/server.nix
    ../../../profiles/state.nix
    ../../../profiles/tailscale.nix
    ../../../profiles/zram.nix
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
    systemd.emergencyAccess = config.users.users.${adminUser.name}.hashedPassword;
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
    # restrictedMacs = [
    #   "5c:e0:c5:8a:24:6a"
    #   "b4:18:d1:ab:4e:5a"
    # ];
    # upstreamDnsServers = [
    #   "2a07:a8c1::"
    #   "45.90.30.0"
    #   "2a07:a8c0::"
    #   "45.90.28.0"
    # ];
    externalInterface = "enp1s0f0";
    internalInterface = "enp2s0";
    # internalInterfaceIP = "192.168.20.1";
    # dnsMasqSettings.no-resolv = true;
    # dnsMasqSettings.bogus-priv = true;
    # dnsMasqSettings.strict-order = true;
  };

  ## NAT64 configuration for IPv6-only clients to access IPv4 services
  ## The public IPv4 address is automatically detected from enp1s0f0 at service start.
  ##
  ## Architecture:
  ##   - Jool runs in a separate network namespace called "nat64"
  ##   - A veth pair connects the main namespace to the nat64 namespace
  ##   - Traffic to 64:ff9b::/96 is routed through the namespace
  ##   - Both LAN clients AND the router itself can access IPv4 via NAT64
  networking.jool.enable = true;

  ## Setup script for the NAT64 namespace and veth pair
  environment.systemPackages = [
    pkgs.iproute2
  ];

  ## Service to setup the NAT64 network namespace
  systemd.services.nat64-namespace-setup = {
    description = "Setup NAT64 network namespace";
    wantedBy = ["multi-user.target"];
    before = ["jool-nat64-default.service"];
    after = ["network-online.target" "systemd-networkd.service"];
    wants = ["network-online.target"];

    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;

      ExecStart = pkgs.writeShellScript "setup-nat64-namespace" ''
        set -euo pipefail

        # Create namespace if it doesn't exist
        ${pkgs.iproute2}/bin/ip netns add nat64 2>/dev/null || true

        # Create veth pair if it doesn't exist
        if ! ${pkgs.iproute2}/bin/ip link show veth-nat64 >/dev/null 2>&1; then
          ${pkgs.iproute2}/bin/ip link add veth-nat64 type veth peer name veth-nat64-ns
          ${pkgs.iproute2}/bin/ip link set veth-nat64-ns netns nat64
        fi

        # Configure main namespace side (both IPv6 and IPv4)
        ${pkgs.iproute2}/bin/ip addr flush dev veth-nat64 || true
        ${pkgs.iproute2}/bin/ip addr add 2001:db8:64::1/64 dev veth-nat64
        ${pkgs.iproute2}/bin/ip addr add 198.51.100.1/30 dev veth-nat64
        ${pkgs.iproute2}/bin/ip link set veth-nat64 up

        # Configure namespace side (both IPv6 and IPv4)
        ${pkgs.iproute2}/bin/ip netns exec nat64 ${pkgs.iproute2}/bin/ip addr flush dev veth-nat64-ns || true
        ${pkgs.iproute2}/bin/ip netns exec nat64 ${pkgs.iproute2}/bin/ip addr add 2001:db8:64::2/64 dev veth-nat64-ns
        ${pkgs.iproute2}/bin/ip netns exec nat64 ${pkgs.iproute2}/bin/ip addr add 198.51.100.2/30 dev veth-nat64-ns
        ${pkgs.iproute2}/bin/ip netns exec nat64 ${pkgs.iproute2}/bin/ip link set veth-nat64-ns up
        ${pkgs.iproute2}/bin/ip netns exec nat64 ${pkgs.iproute2}/bin/ip link set lo up

        # Add route in main namespace to send NAT64 traffic to the namespace
        ${pkgs.iproute2}/bin/ip route replace 64:ff9b::/96 via 2001:db8:64::2 dev veth-nat64

        # Add default IPv4 route in namespace to send translated packets back to main
        ${pkgs.iproute2}/bin/ip netns exec nat64 ${pkgs.iproute2}/bin/ip route replace default via 198.51.100.1 dev veth-nat64-ns

        # Enable IPv4 and IPv6 forwarding in the namespace
        ${pkgs.iproute2}/bin/ip netns exec nat64 ${pkgs.procps}/bin/sysctl -q -w net.ipv4.ip_forward=1
        ${pkgs.iproute2}/bin/ip netns exec nat64 ${pkgs.procps}/bin/sysctl -q -w net.ipv6.conf.all.forwarding=1

        echo "NAT64 namespace setup complete"
      '';

      ExecStop = pkgs.writeShellScript "cleanup-nat64-namespace" ''
        # Remove route
        ${pkgs.iproute2}/bin/ip route del 64:ff9b::/96 via 2001:db8:64::2 dev veth-nat64 2>/dev/null || true

        # Delete veth pair (automatically removes from namespace too)
        ${pkgs.iproute2}/bin/ip link del veth-nat64 2>/dev/null || true

        # Delete namespace
        ${pkgs.iproute2}/bin/ip netns del nat64 2>/dev/null || true

        echo "NAT64 namespace cleanup complete"
      '';
    };
  };

  ## Modified Jool service to run in the namespace
  systemd.services.jool-nat64-default = {
    after = ["nat64-namespace-setup.service"];
    requires = ["nat64-namespace-setup.service"];
    wants = ["network-online.target"];

    serviceConfig = {
      # Run in the NAT64 namespace - systemd handles the namespace isolation
      NetworkNamespacePath = "/var/run/netns/nat64";

      ExecStartPre = [
        # Module needs to be loaded in main namespace first (+ prefix runs in main namespace)
        "+${pkgs.kmod}/bin/modprobe jool"
        # Generate config (+ prefix runs in main namespace to access enp1s0f0)
        "+${generateJoolConfig}"
      ];

      ExecStart = lib.mkForce "${pkgs.jool-cli}/bin/jool file handle /run/jool-nat64-default.conf";
      ExecStop = lib.mkForce "${pkgs.jool-cli}/bin/jool instance remove default";
    };
  };

  # services.prometheus.exporters = {
  #   dnsmasq = {
  #     enable = true;
  #     dnsmasqListenAddress = "localhost:5342";
  #   };
  # };

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
        # {
        #   job_name = "dnsmasq";
        #   scrape_interval = "10s";
        #   static_configs = [
        #     {targets = ["127.0.0.1:9153"];}
        #   ];
        #   inherit relabel_configs;
        # }
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
      file = ../../../secrets/ts-google-9k.age;
      owner = "1337";
    };
    nextdns = {
      file = ../../../secrets/nextdns.age;
    };
  };

  users.users.${adminUser.name}.shell = lib.mkForce pkgs.bashInteractive;

  home-manager = {
    users.${adminUser.name} = {
      imports = [../../../users/profiles/minimal.nix];
    };
  };
}
