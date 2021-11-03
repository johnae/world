{inputs, lib, ...}:
{
  imports = [
    inputs.nixos-hardware.nixosModules.raspberry-pi-4
  ];
  #boot.loader.systemd-boot.enable = lib.mkForce false;
  boot.loader.generic-extlinux-compatible.enable = false;
  boot.loader.raspberryPi = {
    enable = true;
    version = 4;
  };
}
