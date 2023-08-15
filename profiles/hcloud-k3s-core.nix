{
  modulesPath,
  hostName,
  pkgs,
  inputs,
  config,
  lib,
  ...
}: {
  environment.systemPackages = [
    pkgs.k3s
    pkgs.kubectl
    pkgs.tailscale
    pkgs.cryptsetup
  ];
}
