{inputs, ...}: {
  imports = [
    ./amd.nix
    inputs.nixos-hardware.nixosModules.common-cpu-amd
  ];

  boot.initrd.availableKernelModules = [
    "igb"
    "nvme"
    "ahci"
    "usbhid"
  ];
}
