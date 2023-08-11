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
  ];

  services.cloud-init.enable = true;
}
