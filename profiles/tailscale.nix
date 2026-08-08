{config, lib, ...}: {
  services.tailscale = {
    enable = true;
    interfaceName = "tailscale0";
  };
  # Tailscale's tun MTU (1280) plus WireGuard overhead needs an underlay
  # path of >=1360. On a sub-1360 IPv6 path the connection blackholes:
  # WireGuard ignores inbound ICMP PTB by design, so there's no downward
  # PMTUD to shrink the segment. RFC 4821 packetization-layer PMTUD probes
  # the working size from the TCP layer instead of trusting ICMP, which
  # gets long-lived flows (kubectl, SSH) through the blackhole.
  # mkDefault so hosts pulling this in alongside a module that also sets it
  # (e.g. Jovian's Steam module) don't hit a conflict; the value is the same.
  boot.kernel.sysctl."net.ipv4.tcp_mtu_probing" = lib.mkDefault 1;
  systemd.services.tailscaled.unitConfig = lib.mkIf config.ephemeralRoot {
    RequiresMountsFor = "/var/lib/tailscale";
  };
  networking.firewall.trustedInterfaces = ["tailscale0"];
  networking.firewall.checkReversePath = "loose";
}
