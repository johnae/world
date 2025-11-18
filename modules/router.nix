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
  };
}
