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
  ];

  services.myk3s.enable = true;
  services.myk3s.docker = true;

  services.openssh.enable = true;
  services.openssh.passwordAuthentication = false;

  networking.firewall.allowedTCPPortRanges = [
    {
      from = 10250;
      to = 10252;
    }
  ];
  networking.firewall.allowedTCPPorts = [ 22 80 443 6443 ];

  programs.fish.enable = true;
  security.sudo.wheelNeedsPassword = false;

}
