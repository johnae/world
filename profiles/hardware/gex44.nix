{inputs, ...}: {
  imports = [
    ./intel.nix
    inputs.nixos-hardware.nixosModules.common-cpu-intel
  ];

  boot.initrd.availableKernelModules = [
    "igb"
    "r8169" ## this is the network card in gex44
    "i915"
    "nvme"
    "ahci"
    "usbhid"
  ];
}
