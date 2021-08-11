{pkgs, ...}:
{
  services.unifi = {
   enable = true;
   unifiPackage = pkgs.unifi;
  };
  environment.state."/keep".directories = [
    "/var/lib/unifi"
  ];
  networking.firewall.allowedTCPPorts = [ 8443 ];
}
