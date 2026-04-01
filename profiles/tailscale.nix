{config, lib, ...}: {
  services.tailscale = {
    enable = true;
    interfaceName = "tailscale0";
  };
  systemd.services.tailscaled.unitConfig = lib.mkIf config.ephemeralRoot {
    RequiresMountsFor = "/var/lib/tailscale";
  };
  networking.firewall.trustedInterfaces = ["tailscale0"];
  networking.firewall.checkReversePath = "loose";
}
