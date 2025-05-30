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
    #disableDns = mkEnableOption "Whether to disable dns server";
    useNextDns = mkEnableOption "Whether to use nextdns DoH for name resolution";
    nextDnsEnvFile = mkOption {
      type = nullOr str;
      example = "/path/to/envfile";
      default = null;
    };
    upstreamDnsServers = mkOption {
      type = listOf str;
      description = "List of upstream dns server addresses.";
    };
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

    networking.nat = {
      enable = true;
      inherit (cfg) externalInterface;
      internalInterfaces = internalInterfaceNames;
    };

    environment.persistence."/keep".directories = ["/var/lib/dnsmasq"];

    systemd.timers.kill-nextdns = {
      description = "Kill nextdns 5 minutes after boot. What a hack.";
      wantedBy = ["timers.target"];
      timerConfig.OnBootSec = "5m";
    };
    systemd.services.kill-nextdns = {
      description = "Kill nextdns 5 minutes after boot. What a hack.";
      after = ["network-online.target"];
      wants = ["network-online.target"];
      wantedBy = ["multi-user.target"];
      serviceConfig = {
        Type = "oneshot";
        ExecStartPre = "/run/current-system/sw/bin/pkill -9 nextdns";
        ExecStart = "/run/current-system/sw/bin/systemctl restart nextdns";
      };
    };

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
          }
        ];
      };
    };

    services.dnsmasq.enable = true;
    services.dnsmasq.resolveLocalQueries = true;
    services.dnsmasq.settings =
      {
        dhcp-range = mapAttrsToList (tag: net: "${tag},${net.base}.10,${net.base}.128,255.255.255.0,24h") internalInterfaces;
        dhcp-option = (mapAttrsToList (tag: net: "${tag},option:router,${net.address}") internalInterfaces) ++ ["option:dns-server,${cfg.internalInterfaceIP}"];
        interface = internalInterfaceNames;
      }
      // {
        server = mkIf (!cfg.useNextDns) cfg.upstreamDnsServers;
        # server = mkMerge [
        #   (mkIf (!cfg.useNextDns) cfg.upstreamDnsServers)
        #   (mkIf cfg.useNextDns ["127.0.0.1#5555"])
        # ];
        dhcp-authoritative = true;
        dhcp-leasefile = "/var/lib/dnsmasq/dnsmasq.leases";
        add-mac = "text";
        add-subnet = "32,128";
        port = 5342;
      }
      // cfg.dnsMasqSettings;

    services.resolved.enable = false;
    services.nextdns.enable = cfg.useNextDns;
    services.nextdns.arguments = (flatten (map (mac: ["-profile" "${mac}=\${KIDSDNS_ID}"]) cfg.restrictedMacs)) ++ ["-profile" "${cfg.internalInterfaceIP}/24=\${NEXTDNS_ID}" "-cache-size" "10MB" "-discovery-dns" "127.0.0.1:5342" "-report-client-info" "-listen" "${cfg.internalInterfaceIP}:53" "-listen" "127.0.0.1:53"];
    systemd.services.nextdns = mkIf cfg.useNextDns {
      serviceConfig.EnvironmentFile = cfg.nextDnsEnvFile;
      after = ["systemd-networkd-wait-online.service"];
    };

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
