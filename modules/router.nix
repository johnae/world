{
  inputs,
  config,
  lib,
  pkgs,
  hostConfigurations,
  ...
}: let
  inherit
    (lib)
    mkIf
    mkForce
    splitString
    nameValuePair
    mapAttrs'
    mkOption
    mkEnableOption
    mapAttrsToList
    recursiveUpdate
    ;
  inherit
    (builtins)
    head
    tail
    foldl'
    attrValues
    attrNames
    mapAttrs
    concatStringsSep
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
    dnsMasqExtraConfig = mkOption {
      type = lines;
      description = "Extra dnsmasq config";
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

    services.dhcpd4.enable = true;
    services.dhcpd4.interfaces = internalInterfaceNames;
    services.dhcpd4.extraConfig =
      ''
        option subnet-mask 255.255.255.0;
      ''
      + (concatStringsSep "\n" (mapAttrsToList (iface: config: ''
          subnet ${config.network} netmask ${config.netmask} {
            option broadcast-address ${config.base}.255;
            option domain-name-servers ${config.address};
            option routers ${config.address};
            interface ${iface};
            default-lease-time 86400;
            max-lease-time 86400;
            range ${config.base}.10 ${config.base}.128;
          }
        '')
        internalInterfaces));

    environment.state."/keep".directories = ["/var/lib/dnsmasq"];

    services.dnsmasq.enable = true;
    services.dnsmasq.resolveLocalQueries = true;
    services.dnsmasq.settings.server = cfg.upstreamDnsServers;
    services.dnsmasq.extraConfig = cfg.dnsMasqExtraConfig;

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
