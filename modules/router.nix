{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkEnableOption mkIf mkOption;

  cfg = config.services.jae.router;
in {
  options.services.jae.router = with lib.types; {
    enable = mkEnableOption "Whether to enable the IPv6 NAT64 router";

    externalInterface = mkOption {
      type = str;
      example = "eth0";
      description = "The external (WAN) interface.";
    };

    internalInterface = mkOption {
      type = str;
      example = "eth1";
      description = "The internal (LAN) interface.";
    };

    blockySettings = mkOption {
      type = attrs;
      default = {};
      description = "Additional Blocky DNS configuration. See https://0xerr0r.github.io/blocky/latest/configuration/";
      example = lib.literalExpression ''
        {
          blocking = {
            denylists = {
              ads = ["https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts"];
            };
            clientGroupsBlock = {
              default = ["ads"];
            };
          };
        }
      '';
    };

    enableNat64 = mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable NAT64 (Jool) in a separate namespace for IPv4 access from IPv6-only clients.";
    };
  };

  config = mkIf cfg.enable {
    networking.useDHCP = false;
    networking.firewall.trustedInterfaces = [cfg.internalInterface];

    ## Network configuration - IPv6 with DHCPv6 Prefix Delegation
    systemd.network = {
      enable = true;
      networks = {
        # WAN - get IPv4 via DHCP, IPv6 via SLAAC + DHCPv6-PD
        "10-wan" = {
          matchConfig.Name = cfg.externalInterface;
          networkConfig.DHCP = "yes";
          networkConfig.IPv6AcceptRA = "yes";
          networkConfig.DHCPPrefixDelegation = "yes";
          dhcpV6Config = {
            WithoutRA = "no";
          };
        };

        # LAN - delegate /64 from WAN's delegated prefix
        "11-lan" = {
          matchConfig.Name = cfg.internalInterface;
          networkConfig.DHCPPrefixDelegation = "yes";
          networkConfig.IPv6SendRA = "yes";
          # Router gets ::1 from delegated prefix automatically
        };
      };
    };

    ## Persistence
    environment.persistence."/keep".directories = ["/var/lib/dnsmasq"];

    ## NAT64 (Jool) should be configured per-host in the host configuration
    ## See configurations/nixos/x86_64-linux/sagittarius.nix for example

    ## Router Advertisement (corerad)
    ## Announces delegated prefix to LAN clients
    services.corerad = {
      enable = true;
      settings = {
        debug = {
          address = "localhost:9430";
          prometheus = true;
        };
        interfaces = [
          {
            name = cfg.internalInterface;
            advertise = true;
            prefix = [
              {
                prefix = "::/64"; # Uses delegated prefix
              }
            ];
            rdnss = [
              {
                servers = ["::"]; # This router (link-local)
                lifetime = "auto";
              }
            ];
            # Set "Managed" flag - DHCPv6 handles address assignment
            # Devices can still use SLAAC if they prefer (dual mode)
            managed = true;
          }
        ];
      };
    };

    ## DNS: Blocky (filtering) -> CoreDNS (DNS64) -> Upstream DoT
    ## Blocky on port 53, CoreDNS on port 5300
    services.blocky = {
      enable = true;
      settings = lib.mkMerge [
        {
          # Port configuration
          ports = {
            dns = 53;
            http = 4000; # API/metrics
          };

          # Upstream: forward to CoreDNS for DNS64
          upstreams = {
            groups = {
              default = ["127.0.0.1:5300"];
            };
          };

          # Bootstrap DNS (for initial queries before Blocky is ready)
          bootstrapDns = {
            upstream = "https://one.one.one.one/dns-query";
            ips = ["1.1.1.1" "1.0.0.1"];
          };

          # Caching
          caching = {
            minTime = "5m";
            maxTime = "30m";
            prefetching = true;
          };

          # Logging
          log = {
            level = "info";
            format = "text";
          };
        }
        cfg.blockySettings
      ];
    };

    ## DNS64: CoreDNS on port 5300
    ## Translates A records to AAAA using 64:ff9b::/96 prefix
    services.coredns = {
      enable = true;
      config = ''
        .:5300 {
            bind 127.0.0.1 ::1
            dns64 64:ff9b::/96
            forward . tls://2606:4700:4700::1111 tls://2606:4700:4700::1001 {
                tls_servername 1dot1dot1dot1.cloudflare-dns.com
            }
            cache 300
            log
        }
      '';
    };

    ## DHCPv6: Stateful address assignment for devices without SLAAC
    ## dnsmasq provides DHCPv6 only (DNS disabled, handled by Blocky)
    services.dnsmasq = {
      enable = true;
      resolveLocalQueries = false; # Blocky handles DNS
      settings = {
        # Disable DNS server (port 0 = DNS disabled, DHCPv6 only)
        port = 0;

        # Stateful DHCPv6 - assign addresses from ::1000 to ::1fff range
        # SLAAC devices can still use ::2000+ or EUI-64 addresses
        # "constructor:" automatically uses the delegated prefix
        dhcp-range = ["tag:${cfg.internalInterface},::1000,::1fff,constructor:${cfg.internalInterface},64,12h"];

        # Provide DNS server via DHCPv6 option (:: = this router's link-local)
        dhcp-option = ["option6:dns-server,[::]"];

        # Interface configuration
        interface = [cfg.internalInterface];
        except-interface = [cfg.externalInterface];

        # DHCPv6 settings
        dhcp-authoritative = true;
        dhcp-leasefile = "/var/lib/dnsmasq/dnsmasq.leases";

        # Logging for DHCPv6
        log-dhcp = true;
      };
    };

    ## Disable systemd-resolved (conflicts with our DNS setup)
    services.resolved.enable = false;

    ## Kernel parameters for IPv6 routing
    boot.kernel.sysctl = {
      "net.ipv4.conf.all.forwarding" = true;
      "net.ipv6.conf.all.forwarding" = true;

      "net.ipv6.conf.all.accept_ra" = 1;
      "net.ipv6.conf.all.autoconf" = 1;
      "net.ipv6.conf.all.use_tempaddr" = 0;

      # Allow IPv6 autoconfiguration on WAN
      "net.ipv6.conf.${cfg.externalInterface}.accept_ra" = 2;
      "net.ipv6.conf.${cfg.externalInterface}.autoconf" = 1;
    };

    ## NAT64 (Jool) configuration - runs in separate namespace
    ## Allows IPv6-only clients to access IPv4 services
    networking.jool.enable = mkIf cfg.enableNat64 true;

    ## Jool NAT64 config - uses veth IP in pool4
    ## Main namespace will MASQUERADE to public IP via conntrack
    environment.etc."jool-nat64-default.conf" = mkIf cfg.enableNat64 {
      text = builtins.toJSON {
        instance = "default";
        framework = "netfilter";
        global = {
          pool6 = "64:ff9b::/96";
          manually-enabled = true;
        };
        pool4 = [
          {
            protocol = "TCP";
            prefix = "192.168.64.2/32";
            "port range" = "10000-65535";
          }
          {
            protocol = "UDP";
            prefix = "192.168.64.2/32";
            "port range" = "10000-65535";
          }
          {
            protocol = "ICMP";
            prefix = "192.168.64.2/32";
            "port range" = "10000-65535";
          }
        ];
      };
    };

    ## MASQUERADE traffic from NAT64 namespace to appear from public IP
    networking.firewall.extraCommands = mkIf cfg.enableNat64 ''
      # Allow forwarding from NAT64 namespace
      iptables -A FORWARD -i veth-nat64 -o ${cfg.externalInterface} -j ACCEPT
      iptables -A FORWARD -i ${cfg.externalInterface} -o veth-nat64 -m state --state RELATED,ESTABLISHED -j ACCEPT

      # MASQUERADE traffic from namespace (192.168.64.0/30 private range) to public IP
      iptables -t nat -A POSTROUTING -s 192.168.64.0/30 -o ${cfg.externalInterface} -j MASQUERADE
    '';

    networking.firewall.extraStopCommands = mkIf cfg.enableNat64 ''
      # Cleanup rules
      iptables -D FORWARD -i veth-nat64 -o ${cfg.externalInterface} -j ACCEPT 2>/dev/null || true
      iptables -D FORWARD -i ${cfg.externalInterface} -o veth-nat64 -m state --state RELATED,ESTABLISHED -j ACCEPT 2>/dev/null || true
      iptables -t nat -D POSTROUTING -s 192.168.64.0/30 -o ${cfg.externalInterface} -j MASQUERADE 2>/dev/null || true
    '';

    ## Service to setup the NAT64 network namespace
    systemd.services.nat64-namespace-setup = mkIf cfg.enableNat64 {
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
          # IPv6: ULA (Unique Local Address) - fd00::/8 prefix
          # IPv4: RFC 1918 private address (192.168.0.0/16) - home network range
          ${pkgs.iproute2}/bin/ip addr flush dev veth-nat64 || true
          ${pkgs.iproute2}/bin/ip addr add fd00:64::1/64 dev veth-nat64
          ${pkgs.iproute2}/bin/ip addr add 192.168.64.1/30 dev veth-nat64
          ${pkgs.iproute2}/bin/ip link set veth-nat64 up

          # Configure namespace side (both IPv6 and IPv4)
          ${pkgs.iproute2}/bin/ip netns exec nat64 ${pkgs.iproute2}/bin/ip addr flush dev veth-nat64-ns || true
          ${pkgs.iproute2}/bin/ip netns exec nat64 ${pkgs.iproute2}/bin/ip addr add fd00:64::2/64 dev veth-nat64-ns
          ${pkgs.iproute2}/bin/ip netns exec nat64 ${pkgs.iproute2}/bin/ip addr add 192.168.64.2/30 dev veth-nat64-ns
          ${pkgs.iproute2}/bin/ip netns exec nat64 ${pkgs.iproute2}/bin/ip link set veth-nat64-ns up
          ${pkgs.iproute2}/bin/ip netns exec nat64 ${pkgs.iproute2}/bin/ip link set lo up

          # Add route in main namespace to send NAT64 traffic to the namespace
          ${pkgs.iproute2}/bin/ip route replace 64:ff9b::/96 via fd00:64::2 dev veth-nat64

          # Add default IPv4 route in namespace to send translated packets back to main
          ${pkgs.iproute2}/bin/ip netns exec nat64 ${pkgs.iproute2}/bin/ip route replace default via 192.168.64.1 dev veth-nat64-ns

          # Enable IPv4 and IPv6 forwarding in the namespace
          ${pkgs.iproute2}/bin/ip netns exec nat64 ${pkgs.procps}/bin/sysctl -q -w net.ipv4.ip_forward=1
          ${pkgs.iproute2}/bin/ip netns exec nat64 ${pkgs.procps}/bin/sysctl -q -w net.ipv6.conf.all.forwarding=1

          echo "NAT64 namespace setup complete"
        '';

        ExecStop = pkgs.writeShellScript "cleanup-nat64-namespace" ''
          # Remove route
          ${pkgs.iproute2}/bin/ip route del 64:ff9b::/96 via fd00:64::2 dev veth-nat64 2>/dev/null || true

          # Delete veth pair (automatically removes from namespace too)
          ${pkgs.iproute2}/bin/ip link del veth-nat64 2>/dev/null || true

          # Delete namespace
          ${pkgs.iproute2}/bin/ip netns del nat64 2>/dev/null || true

          echo "NAT64 namespace cleanup complete"
        '';
      };
    };

    ## Jool NAT64 service - runs in the namespace
    systemd.services.jool-nat64-default = mkIf cfg.enableNat64 {
      after = ["nat64-namespace-setup.service"];
      requires = ["nat64-namespace-setup.service"];
      wants = ["network-online.target"];

      serviceConfig = {
        # Run in the NAT64 namespace - systemd handles the namespace isolation
        NetworkNamespacePath = "/var/run/netns/nat64";

        ExecStartPre = [
          # Module needs to be loaded in main namespace first (+ prefix runs in main namespace)
          "+${pkgs.kmod}/bin/modprobe jool"
        ];

        ExecStart = lib.mkForce "${pkgs.jool-cli}/bin/jool file handle /etc/jool-nat64-default.conf";
        ExecStop = lib.mkForce "${pkgs.jool-cli}/bin/jool instance remove default";
      };
    };
  };
}
