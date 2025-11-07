{
  config,
  lib,
  ...
}: let
  inherit
    (lib)
    flatten
    mapAttrsToList
    mkEnableOption
    mkIf
    mkOption
    splitString
    ;
  inherit
    (builtins)
    attrNames
    head
    tail
    ;

  cfg = config.services.jae.router;

  ipBase = ip: let
    s = splitString "." ip;
    a = head s;
    b = head (tail s);
    c = head (tail (tail s));
  in "${a}.${b}.${c}";

  internalInterfaces = {
    ${cfg.internalInterface} = rec {
      base = ipBase cfg.internalInterfaceIP;
      address = "${base}.1";
      network = "${base}.0";
      prefixLength = 24;
      netmask = "255.255.255.0";
    };
  };

  internalInterfaceNames = attrNames internalInterfaces;
in {
  options.services.jae.router = with lib.types; {
    enable = mkEnableOption "Whether to enable the router";
    upstreamDnsServers = mkOption {
      type = listOf str;
      description = "List of upstream dns server addresses.";
    };
    # disableIPv4 = mkOption {
    #   type = bool;
    #   description = "If ipv4 should be disabled on the local network.";
    # };
    restrictedMacs = mkOption {
      type = listOf str;
      description = "List of mac addresses.";
      default = [];
    };
    dnsMasqSettings = mkOption {
      type = attrsOf anything;
      description = "Extra dnsmasq settings";
    };
    externalInterface = mkOption {
      type = str;
      example = "eth0";
      description = "The external interface.";
    };
    internalInterface = mkOption {
      type = str;
      example = "eth1";
      description = "The internal interface.";
    };
    internalInterfaceIP = mkOption {
      type = str;
      example = "192.168.1.1";
      default = "192.168.1.1";
      description = "The internal interface ip.";
    };
  };

  config = mkIf cfg.enable {
    networking.useDHCP = false;
    networking.firewall.trustedInterfaces = internalInterfaceNames;

    systemd.network = {
      enable = true;
      networks =
        {
          "10-wan" = {
            matchConfig.Name = cfg.externalInterface;
            networkConfig.DHCP = "yes";
            networkConfig.IPv6AcceptRA = "yes";
            networkConfig.DNS = "127.0.0.1";
            networkConfig.DHCPPrefixDelegation = "yes";
            dhcpV6Config = {
              WithoutRA = "no";
            };
          };
        }
        // (lib.mapAttrs' (name: net: {
            name = "11-${name}";
            value = {
              matchConfig.Name = name;
              networkConfig.DHCPPrefixDelegation = "yes";
              networkConfig.IPv6SendRA = "yes";
              addresses = [
                {
                  Address = "${net.address}/${toString net.prefixLength}";
                }
              ];
            };
          })
          internalInterfaces);
    };

    # networking.nat = {
    #   enable = !cfg.disableIPv4;
    #   inherit (cfg) externalInterface;
    #   internalInterfaces = internalInterfaceNames;
    # };

    environment.persistence."/keep".directories = ["/var/lib/dnsmasq"];

    ## NAT64 (Jool) should be configured per-host in the host configuration
    ## See configurations/nixos/x86_64-linux/sagittarius.nix for example

    ## enable ipv6 on local network
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
                prefix = "::/64";
              }
            ];
            rdnss = [
              {
                servers = ["::"];
                lifetime = "auto";
              }
            ];
            # Set "Managed" flag to indicate DHCPv6 handles address assignment
            # Devices can still use SLAAC if they prefer (dual mode)
            managed = true;
          }
        ];
      };
    };

    #     . {
    #     bind ::
    #     dns64 64:ff9b::/96
    #     forward . tls://2606:4700:4700::1111 {
    #         tls_servername 1dot1dot1dot1.cloudflare-dns.com
    #     }
    #     cache 30
    #     log
    #     errors
    # }

    services.coredns = {
      enable = true;
      config = ''
        . {
            bind ::
            dns64 64:ff9b::/96
            forward . tls://2606:4700:4700::1111 {
                tls_servername 1dot1dot1dot1.cloudflare-dns.com
            }
            cache 300
            log
        }
      '';
    };

    ## DHCPv6 for devices that need address assignment (stateful mode)
    ## corerad sends RA with M-flag, supporting both:
    ##   - SLAAC devices (can still autoconfigure)
    ##   - DHCPv6-only devices (get addresses from dnsmasq)
    services.dnsmasq.enable = true;
    services.dnsmasq.resolveLocalQueries = false;  # CoreDNS handles DNS
    services.dnsmasq.settings = {
      # Disable DNS server functionality (port 0 = DNS disabled, DHCPv6 only)
      port = 0;

      # Stateful DHCPv6 - assign addresses from ::1000 to ::1fff range
      # SLAAC devices can still use ::2000+ or EUI-64 addresses
      # The "constructor:" prefix uses the actual IPv6 prefix on the interface
      dhcp-range = mapAttrsToList (tag: net: "tag:${tag},::1000,::1fff,constructor:${tag},64,12h") internalInterfaces;

      # Provide DNS server via DHCPv6 option (:: means this server's link-local address)
      dhcp-option = ["option6:dns-server,[::]"];

      # Interface configuration
      interface = internalInterfaceNames;
      except-interface = cfg.externalInterface;

      # DHCPv6 settings
      dhcp-authoritative = true;
      dhcp-leasefile = "/var/lib/dnsmasq/dnsmasq.leases";

      # Logging for DHCPv6
      log-dhcp = true;
    } // cfg.dnsMasqSettings;

    services.resolved.enable = false;
    # services.nextdns.enable = cfg.useNextDns;
    # services.nextdns.arguments = (flatten (map (mac: ["-profile" "${mac}=\${KIDSDNS_ID}"]) cfg.restrictedMacs)) ++ ["-profile" "${cfg.internalInterfaceIP}/24=\${NEXTDNS_ID}" "-cache-size" "10MB" "-discovery-dns" "127.0.0.1:5342" "-report-client-info" "-listen" "${cfg.internalInterfaceIP}:53" "-listen" "127.0.0.1:53"];
    # systemd.services.nextdns = mkIf cfg.useNextDns {
    #   serviceConfig.EnvironmentFile = cfg.nextDnsEnvFile;
    #   after = ["systemd-networkd-wait-online.service"];
    # };

    boot.kernel.sysctl."net.ipv4.conf.all.forwarding" = true;
    boot.kernel.sysctl."net.ipv6.conf.all.forwarding" = true;

    boot.kernel.sysctl."net.ipv6.conf.all.accept_ra" = 1;
    boot.kernel.sysctl."net.ipv6.conf.all.autoconf" = 1;
    boot.kernel.sysctl."net.ipv6.conf.all.use_tempaddr" = 0;

    # allow ipv6 autoconfiguration and temporary address use on wan
    boot.kernel.sysctl."net.ipv6.conf.${cfg.externalInterface}.accept_ra" = 2;
    boot.kernel.sysctl."net.ipv6.conf.${cfg.externalInterface}.autoconf" = 1;
  };
}
