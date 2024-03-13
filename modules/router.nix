{
  config,
  lib,
  ...
}: let
  inherit
    (lib)
    mkIf
    mkMerge
    splitString
    mkOption
    mkEnableOption
    mapAttrsToList
    ;
  inherit
    (builtins)
    head
    tail
    attrNames
    mapAttrs
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
    dnsMasqSettings = mkOption {
      type = attrsOf anything;
      description = "Extran dnsmasq settings";
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

    networking.interfaces =
      {
        ${cfg.externalInterface}.useDHCP = true;
      }
      // (
        mapAttrs (_: net: {
          useDHCP = false;
          ipv4.addresses = [{inherit (net) address prefixLength;}];
        })
        internalInterfaces
      );

    networking.nat.enable = true;
    networking.nat.externalInterface = cfg.externalInterface;
    networking.nat.internalInterfaces = internalInterfaceNames;

    environment.persistence."/keep".directories = ["/var/lib/dnsmasq"];

    services.dnsmasq.enable = true;
    services.dnsmasq.resolveLocalQueries = true;
    services.dnsmasq.settings =
      {
        dhcp-range = mapAttrsToList (tag: net: "${tag},${net.base}.10,${net.base}.128,255.255.255.0,24h") internalInterfaces;
        dhcp-option = mapAttrsToList (tag: net: "${tag},option:router,${net.address}") internalInterfaces;
        interface = internalInterfaceNames;
      }
      // {
        server = mkMerge [
          (mkIf (!cfg.useNextDns) cfg.upstreamDnsServers)
          (mkIf cfg.useNextDns ["127.0.0.1:5555"])
        ];
        dhcp-authoritative = true;
        dhcp-leasefile = "/var/lib/dnsmasq/dnsmasq.leases";
        add-mac = true;
        add-subnet = "32,128";
      }
      // cfg.dnsMasqSettings;

    services.nextdns.enable = cfg.useNextDns;
    services.nextdns.arguments = ["-profile" "${cfg.internalInterfaceIP}/24=\${NEXTDNS_ID}" "-cache-size" "10MB" "-listen" "127.0.0.1:5555"];
    systemd.services.nextdns = mkIf cfg.useNextDns {
      serviceConfig.EnvironmentFile = cfg.nextDnsEnvFile;
    };

    boot.kernel.sysctl."net.ipv4.conf.all.forwarding" = true;
    boot.kernel.sysctl."net.ipv6.conf.all.forwarding" = true;

    # don't automatically configure any ipv6 addresses
    boot.kernel.sysctl."net.ipv6.conf.all.accept_ra" = 0;
    boot.kernel.sysctl."net.ipv6.conf.all.autoconf" = 0;
    boot.kernel.sysctl."net.ipv6.conf.all.use_tempaddr" = 0;

    # allow ipv6 autoconfiguration and temporary address use on wan
    boot.kernel.sysctl."net.ipv6.conf.${cfg.externalInterface}.accept_ra" = 2;
    boot.kernel.sysctl."net.ipv6.conf.${cfg.externalInterface}.autoconf" = 1;
  };
}
