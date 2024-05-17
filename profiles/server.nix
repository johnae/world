{
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (inputs) nixos-hardware;
in {
  imports = [
    "${nixos-hardware}/common/pc/ssd"
    ./defaults.nix
  ];

  boot.kernel.sysctl = {
    "fs.inotify.max_queued_events" = lib.mkDefault 32768;
    "fs.inotify.max_user_instances" = lib.mkDefault 8192;
    "fs.inotify.max_user_watches" = lib.mkDefault 495486;
    "net.core.rmem_max" = lib.mkDefault 2500000; ## https://github.com/lucas-clemente/quic-go/wiki/UDP-Receive-Buffer-Size
  };

  networking.usePredictableInterfaceNames = true; ## works when there's only one ethernet port
  networking.useDHCP = false;

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
    pkgs.wireguard-tools
    pkgs.nfs-utils
    pkgs.iptables
  ];

  networking.search = lib.mkForce [];
  networking.domain = lib.mkForce null;

  services.openssh.enable = true;
  services.openssh.settings.PasswordAuthentication = false;

  networking.firewall.allowedTCPPorts = [22];

  programs.fish.enable = true;
  security.sudo.wheelNeedsPassword = false;

  machinePurpose = lib.mkForce "server";
}
