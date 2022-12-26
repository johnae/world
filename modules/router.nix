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

  internalInterfaces =
    {
      ${cfg.internalInterface} = rec {
        base = ipBase cfg.internalInterfaceIP;
        address = "${base}.1";
        network = "${base}.0";
        prefixLength = 24;
        netmask = "255.255.255.0";
      };
    }
    // (
      mapAttrs (_: vlan: rec {
        base = "192.168.${toString vlan.id}";
        address = "${base}.1";
        network = "${base}.0";
        prefixLength = 24;
        netmask = "255.255.255.0";
      })
      cfg.vlans
    );

  internalInterfaceNames = attrNames internalInterfaces;

  hosts = mkIf (cfg.innernetServer != null) (
    let
      peers = foldl' (x: y: x // y.settings.peers) {} (attrValues hostConfigurations.${cfg.innernetServer}.services.innernet.server);
    in
      (mapAttrs' (name: conf: nameValuePair conf.ip [name "${name}.${cfg.domain}"]) peers)
      // {
        ${cfg.innernetServerInternalIp} = [cfg.innernetServer "${cfg.innernetServer}.${cfg.domain}"];
        ${cfg.unifiAddress} = ["unifi" "unifi.${cfg.domain}"];
        "127.0.0.2" = mkForce ["localhost2"];
        "::1" = mkForce ["localhost2"];
      }
  );
in {
  options.services.jae.router = with lib.types; {
    enable = mkEnableOption "Whether to enable the router";
    upstreamDnsServers = mkOption {
      type = listOf str;
      description = "List of upstream dns server addresses.";
    };
    innernetServer = mkOption {
      type = nullOr str;
      default = null;
      description = "Name of innernet server host";
    };
    innernetServerInternalIp = mkOption {
      type = nullOr str;
      default = null;
      description = "Internal IP of innernet server";
    };
    domain = mkOption {
      type = str;
      example = "example.lan";
      description = "The local network domain.";
    };
    dnsCrypt = mkOption {
      type = bool;
      default = false;
      description = "Whether to use dnscrypt-proxy2";
    };
    dnsmasqAdditionalListenAddresses = mkOption {
      type = listOf str;
      default = [];
      description = "Additional ip:s where dnsmasq should listen";
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
      default = {};
      type = attrsOf (submodule (_: {
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
        option space ubnt;
        option ubnt.unifi-address code 1 = ip-address;

        class "ubnt" {
                match if substring (option vendor-class-identifier, 0, 4) = "ubnt";
                option vendor-class-identifier "ubnt";
                vendor-option-space ubnt;
        }

      ''
      + (concatStringsSep "\n" (mapAttrsToList (iface: config: ''
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
        '')
        internalInterfaces));

    environment.state."/keep".directories = ["/var/lib/dnsmasq"];

    services.dnscrypt-proxy2 = {
      enable = cfg.dnsCrypt;
      settings = {
        bootstrap_resolvers = ["1.1.1.1:53" "1.0.0.1:53"];
        require_dnssec = true;
        require_nolog = true;
        require_nofilter = true;
        block_ipv6 = true;
        ipv6_servers = false;
        cache_size = 4096;
        cache_min_ttl = 2400;
        cache_max_ttl = 86400;
        cache_neg_min_ttl = 30;
        cache_neg_max_ttl = 600;
        timeout = 2500;
        keepalive = 30;
        ignore_system_dns = true;
        dnscrypt_servers = true;
        doh_servers = true;
        listen_addresses = ["127.0.0.1:5300"];
        sources.public-resolvers = {
          urls = [
            "https://raw.githubusercontent.com/DNSCrypt/dnscrypt-resolvers/master/v3/public-resolvers.md"
            "https://download.dnscrypt.info/resolvers-list/v3/public-resolvers.md"
          ];
          cache_file = "/var/lib/dnscrypt-proxy2/public-resolvers.md";
          minisign_key = "RWQf6LRCGA9i53mlYecO4IzT51TGPpvWucNSCh1CBM0QTaLn73Y7GFO3";
        };
        server_names = ["cloudflare-security" "cloudflare-security-ipv6"];
        blocked_names.blocked_names_file = "${pkgs.notracking}/dnscrypt-proxy/dnscrypt-proxy.blacklist.txt";
      };
    };

    services.dnsmasq.enable = true;
    services.dnsmasq.resolveLocalQueries = true;
    services.dnsmasq.settings.servers =
      if cfg.dnsCrypt
      then ["127.0.0.1#5300"]
      else cfg.upstreamDnsServers;
    services.dnsmasq.extraConfig = ''
      dnssec
      conf-file=${pkgs.dnsmasq}/share/dnsmasq/trust-anchors.conf
      cache-size=150
      log-queries
      domain-needed
      bogus-priv
      filterwin2k
      no-hosts
      neg-ttl=5
      clear-on-reload
      addn-hosts=${config.environment.etc.hosts.source}
      listen-address=::1,127.0.0.1,${concatStringsSep "," ((mapAttrsToList (_: config: config.address) internalInterfaces) ++ cfg.dnsmasqAdditionalListenAddresses)}

      ${concatStringsSep "\n" (map (iface: "interface=${iface}") internalInterfaceNames)}

      domain=${cfg.domain}
      conf-file=${pkgs.notracking}/dnsmasq/dnsmasq.blacklist.txt
    '';

    networking.hosts = hosts;

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
