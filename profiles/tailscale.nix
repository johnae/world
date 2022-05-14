{
  services.tailscale.enable = true;
  networking.firewall.trustedInterfaces = ["tailscale0"];
  # Strict reverse path filtering breaks Tailscale exit node use and some subnet routing setupsStrict reverse path filtering breaks Tailscale exit node use and some subnet routing setups, see: https://github.com/tailscale/tailscale/issues/4432
  networking.firewall.checkReversePath = "loose";
}
