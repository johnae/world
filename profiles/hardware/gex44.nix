{inputs, ...}: {
  imports = [
    ./intel.nix
    inputs.nixos-hardware.nixosModules.common-cpu-intel
  ];
}
