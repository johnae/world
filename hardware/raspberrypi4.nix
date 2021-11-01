{inputs, ...}:
{
  imports = [
    inputs.nixos-hardware.nixosModules.raspberry-pi-4
  ];
  boot.loader.raspberryPi = {
    enable = true;
    version = 4;
  };
}
