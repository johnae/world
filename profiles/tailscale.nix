{
  services.tailscale = {
    enable = true;
    #useRoutingFeatures = true;
    interfaceName = "tailscale0";
  };
  networking.firewall.trustedInterfaces = ["tailscale0"];
  networking.firewall.checkReversePath = "loose";
  ## the servers will need auth keys
}
