{ config, hostName, importSecret, lib, pkgs, inputs, ... }:
let
  nixos-hardware = inputs.nixos-hardware;
  tailscale = importSecret "${inputs.secrets}/tailscale/meta.nix";
in
{
  imports = [
    "${nixos-hardware}/common/pc/ssd"
    tailscale
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

  services.k3s = {
    enable = true;
    docker = true;
    nodeName = hostName;
    flannelBackend = "none";
    extraFlags = " --flannel-iface=tailscale0 ";
    systemdAfter = [ "tailscaled.service" "tailscale-auth.service" ];
  };

  services.tailscale.enable = true;

  networking.search = lib.mkForce [ ];
  networking.domain = lib.mkForce null;

  services.openssh.enable = true;
  services.openssh.passwordAuthentication = false;

  networking.firewall.allowedTCPPortRanges = [
    {
      from = 10250;
      to = 10252;
    }
  ];
  networking.firewall.allowedTCPPorts = [ 22 ];
  networking.firewall.trustedInterfaces = [ "tailscale0" "cni0" "flannel.1" ];

  programs.fish.enable = true;
  security.sudo.wheelNeedsPassword = false;

}
