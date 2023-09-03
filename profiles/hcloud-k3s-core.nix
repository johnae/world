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
  fileSystems."/etc/ssh" = {
    device = "none";
    fsType = "tmpfs";
    options = ["defaults" "size=2M" "mode=755"];
  };
}
