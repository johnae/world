{
  config,
  lib,
  ...
}: let
  inherit
    (lib)
    mapAttrsToList
    mkEnableOption
    mkIf
    mkOption
    splitString
    ;
  inherit
    (builtins)
    attrNames
    ;

  cfg = config.services.jae.router;

  # Extract first three octets from IP (e.g., "192.168.1.1" -> "192.168.1")
  ipBase = ip: lib.concatStringsSep "." (lib.take 3 (splitString "." ip));

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
      default = ["2620:fe::fe" "9.9.9.9" "2620:fe::9" "149.112.112.112"];
    };
    dnsMasqSettings = mkOption {
      type = attrsOf anything;
      description = "Extra dnsmasq settings";
      default = {};
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

    networking.nat = {
      enable = true;
      inherit (cfg) externalInterface;
      internalInterfaces = internalInterfaceNames;
    };

    environment.persistence."/keep".directories = ["/var/lib/dnsmasq"];

    ## enable jool nat64
    # networking.jool.enable = true;
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
            name = "pref64";
            advertise = true;
            prefix = [
              {
                prefix = "64:ff9b::/96";
              }
            ];
          }
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
          }
        ];
      };
    };

    # dnsmasq for DHCP only
    services.dnsmasq.enable = true;
    services.dnsmasq.resolveLocalQueries = false;
    services.dnsmasq.settings =
      {
        dhcp-range = mapAttrsToList (tag: net: "${tag},${net.base}.10,${net.base}.128,255.255.255.0,24h") internalInterfaces;
        dhcp-option = (mapAttrsToList (tag: net: "${tag},option:router,${net.address}") internalInterfaces) ++ ["option:dns-server,${cfg.internalInterfaceIP}"];
        interface = internalInterfaceNames;
        dhcp-authoritative = true;
        dhcp-leasefile = "/var/lib/dnsmasq/dnsmasq.leases";
        add-mac = "text";
        add-subnet = "32,128";
        port = 0; # Disable DNS, DHCP only
      }
      // cfg.dnsMasqSettings;

    # Blocky for DNS filtering
    services.blocky = {
      enable = true;
      settings = {
        ports.dns = 53;
        upstreams.groups.default = cfg.upstreamDnsServers;
        blocking = {
          denylists = {
            ads = ["https://cdn.jsdelivr.net/gh/hagezi/dns-blocklists@latest/hosts/pro.txt"];
          };
          clientGroupsBlock = {
            default = ["ads"];
          };
        };
        caching = {
          minTime = "5m";
          maxTime = "30m";
          prefetching = true;
        };
      };
    };

    services.resolved.enable = false;

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
