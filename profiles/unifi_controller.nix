{pkgs, lib, hostConfigs, ...}:
let
  inherit (builtins) filter mapAttrs attrValues;
  inherit (lib) attrByPath;
  wgHosts = filter (h: h.ips !=null) (attrValues (mapAttrs (_: h: {
    inherit (h) wgPublicKey;
  } // {
    ips = attrByPath [ "networking" "wireguard" "interfaces" "home" "ips" ] null h;
  }) hostConfigs));
  peer = map (host: {
    "${host.wgPublicKey}" = {
      allowed-ips = host.ips;
      persistent-keepalive = 25;
    };
  }) wgHosts;
in
{
  services.unifi = {
   enable = true;
   unifiPackage = pkgs.unifi;
   configGateway.default.config = {
     firewall.group.network-group.remote_user_vpn_network = {
       description = "Remote Wireguard VPN subnets";
       network = [
         "192.168.241.0/24"
       ];
     };
     interfaces = {
       wireguard = {
         wg0 = {
           description = "Remote User WG VPN";
           address = [
             "192.168.241.1/24"
           ];
           firewall = {
             "in".name = "LAN_IN";
             local.name = "LAN_LOCAL";
             out.name = "LAN_OUT";
           };
           listen-port = "51820";
           mtu = "1352";
           inherit peer;
           private-key = "/config/auth/wireguard/wg_private.key";
           route-allowed-ips = "true";
         };
       };
     };
   };
  };
  environment.state."/keep".directories = [
    "/var/lib/unifi"
  ];
  networking.firewall.allowedTCPPorts = [ 8443 ];
}
