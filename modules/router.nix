{ inputs, config, lib, ... }:

with lib;

let

  inherit (lib) splitString;

  cfg = config.services.jae.router;

  ipBase = ip: let
    s = splitString "." ip;
    a = head s;
    b = head (tail s);
    c = head (tail (tail s));
  in
    "${a}.${b}.${c}";

  internalInterfaces = {
    ${cfg.internalInterface} = rec {
      base = ipBase cfg.internalInterfaceIP;
      address = "${base}.1";
      network = "${base}.0";
      prefixLength = 24;
      netmask = "255.255.255.0";
    };
  } // (
    mapAttrs (_: vlan: rec {
      base = "192.168.${toString vlan.id}";
      address = "${base}.1";
      network = "${base}.0";
      prefixLength = 24;
      netmask = "255.255.255.0";
    }) cfg.vlans
  );

  internalInterfaceNames = attrNames internalInterfaces;

in {

  options.services.jae.router = with lib.types; {
    enable = mkEnableOption "Whether to enable the my router";
    upstreamDnsServers = mkOption {
      type = listOf str;
      description = "List of upstream dns server addresses.";
    };
    domain = mkOption {
      type = str;
      example = "example.lan";
      description = "The local network domain.";
    };
    unifiAddress = mkOption {
      type = str;
      example = "1.2.3.4";
      description = "The address of the unifi controller";
    };
    externalInterface = mkOption {
      type = str;
      example = "eth0";
      description = "The external interface.";
    };
    internalInterface = mkOption {
      type = str;
      example = "eth1";
      description = "The external interface.";
    };
    internalInterfaceIP = mkOption {
      type = str;
      example = "192.168.1.1";
      default = "192.168.1.1";
      description = "The internal interface ip.";
    };
    vlans = mkOption {
      type = attrsOf (submodule ({...}: {
        options = {
          id = mkOption {
            type = int;
            example = "10";
            description = "The vlan id";
          };
          interface = mkOption {
            type = str;
            example = "eth0";
            description = "The physical interface where we create the vlan.";
          };
        };
      }));
      example = ''
      {
        vlans.vlan10 = { id = 10; interface = "eth0"; };
        vlans.vlan20 = { id = 20; interface = "eth0"; };
      }
      '';
    };
  };

  config = mkIf cfg.enable {
    networking.useDHCP = false;
    networking.vlans = cfg.vlans;
    networking.firewall.trustedInterfaces = internalInterfaceNames;

    networking.interfaces = {
      ${cfg.externalInterface}.useDHCP = true;
    } // (
      mapAttrs (_: net: {
        useDHCP = false;
        ipv4.addresses = [ { inherit (net) address prefixLength; } ];
      }) internalInterfaces
    );

    networking.nat.enable = true;
    networking.nat.externalInterface = cfg.externalInterface;
    networking.nat.internalInterfaces = internalInterfaceNames;

    services.dhcpd4.enable = true;
    services.dhcpd4.interfaces = internalInterfaceNames;
    services.dhcpd4.extraConfig = ''
      option subnet-mask 255.255.255.0;
      option space ubnt;
      option ubnt.unifi-address code 1 = ip-address;

      class "ubnt" {
              match if substring (option vendor-class-identifier, 0, 4) = "ubnt";
              option vendor-class-identifier "ubnt";
              vendor-option-space ubnt;
      }

    '' + (concatStringsSep "\n" (mapAttrsToList (iface: config: ''
      subnet ${config.network} netmask ${config.netmask} {
        option broadcast-address ${config.base}.255;
        option domain-name-servers ${config.address};
        option ubnt.unifi-address ${cfg.unifiAddress};
        option routers ${config.address};
        option domain-name "${cfg.domain}";
        option domain-search "${cfg.domain}";
        interface ${iface};
        default-lease-time 86400;
        max-lease-time 86400;
        range ${config.base}.10 ${config.base}.128;
      }
    '') internalInterfaces));

    environment.state."/keep".directories = [ "/var/lib/dnsmasq" ];
    services.dnsmasq.enable = true;
    services.dnsmasq.resolveLocalQueries = true;
    services.dnsmasq.servers = cfg.upstreamDnsServers;
    services.dnsmasq.extraConfig = ''
      cache-size=10000
      log-queries
      domain-needed
      bogus-priv
      filterwin2k
      no-hosts
      addn-hosts=${config.environment.etc.hosts.source}
      listen-address=::1,127.0.0.1,${concatStringsSep "," (mapAttrsToList (_: config: config.address) internalInterfaces)}

      ${concatStringsSep "\n" (map (iface: "interface=${iface}") internalInterfaceNames)}

      domain=${cfg.domain}
      conf-file=${inputs.notracking}/dnsmasq/dnsmasq.blacklist.txt
    '';

    networking.hosts.${cfg.unifiAddress} = [ "unifi" ];

    boot.kernel.sysctl."net.ipv4.conf.all.forwarding" = true;
    boot.kernel.sysctl."net.ipv6.conf.all.forwarding" = true;

    # don't automatically configure any ipv6 addresses
    boot.kernel.sysctl."net.ipv6.conf.all.accept_ra" = 0;
    boot.kernel.sysctl."net.ipv6.conf.all.autoconf" = 0;
    boot.kernel.sysctl."net.ipv6.conf.all.use_tempaddr" = 0;

    # allow ipv6 autoconfiguration and temporary address use on wan
    boot.kernel.sysctl."net.ipv6.conf.enp1s0u2.accept_ra" = 2;
    boot.kernel.sysctl."net.ipv6.conf.enp1s0u2.autoconf" = 1;
  };
}
