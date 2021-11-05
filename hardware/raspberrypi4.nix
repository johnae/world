{inputs, pkgs, lib, ...}:
{
  imports = [
    inputs.nixos-hardware.nixosModules.raspberry-pi-4
  ];
  boot.loader.generic-extlinux-compatible.enable = false;
  ## must use a specific kernel, the lastest doesn't work as of 2021-11-04
  boot.kernelPackages = lib.mkForce pkgs.linuxPackages_rpi4;
  boot.loader.raspberryPi = {
    enable = true;
    version = 4;
  };
}
