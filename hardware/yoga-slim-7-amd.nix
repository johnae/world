{inputs, ...}:
{
  imports = [
    ./amd.nix
    inputs.nixos-hardware.nixosModules.common-cpu-amd
  ];
}

