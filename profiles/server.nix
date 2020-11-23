{ config, lib, pkgs, inputs, ... }:
let
  nixos-hardware = inputs.nixos-hardware;
in
{
  imports = [
    "${nixos-hardware}/common/pc/ssd"
    ./defaults.nix
  ];

  networking.usePredictableInterfaceNames = false; ## works when there's only one ethernet port
  networking.useDHCP = false;

  console.font = "Lat2-Terminus16";

  environment.systemPackages = [
    pkgs.wget
    pkgs.vim
    pkgs.curl
    pkgs.man-pages
    pkgs.cacert
    pkgs.zip
    pkgs.unzip
    pkgs.jq
    pkgs.git
    pkgs.fd
    pkgs.lsof
    pkgs.fish
    pkgs.wireguard
    pkgs.nfs-utils
    pkgs.iptables
  ];

  services.myk3s.enable = true;
  systemd.services.k3s.after = [ "tailscaled.service" ];

  services.openssh.enable = true;
  services.openssh.passwordAuthentication = false;

  networking.firewall.allowedTCPPortRanges = [
    {
      from = 10250;
      to = 10252;
    }
  ];
  networking.firewall.allowedTCPPorts = [ 22 80 443 6443 179 51820 8285 8472 ];
  networking.firewall.allowedUDPPorts = [ 22 80 443 6443 179 51820 8285 8472 ];
  networking.firewall.trustedInterfaces = [ "tailscale0" "cni0" "flannel.1" ];

  programs.fish.enable = true;
  security.sudo.wheelNeedsPassword = false;

}
