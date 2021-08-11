{
  services.unifi.enable = true;
  environment.state."/keep".directories = [
    "/var/lib/unifi"
  ];
  networking.firewall.allowedTCPPorts = [ 8443 ];
}
