{
  inputs,
  config,
  lib,
  ...
}: {
  imports = [
    ./amd.nix
    inputs.nixos-hardware.nixosModules.common-cpu-amd
    inputs.nixos-hardware.nixosModules.common-cpu-amd-pstate
    inputs.nixos-hardware.nixosModules.common-gpu-amd
  ];

  hardware.trackpoint.enable = lib.mkDefault true;
  hardware.trackpoint.emulateWheel = lib.mkDefault config.hardware.trackpoint.enable;
  hardware.enableRedistributableFirmware = lib.mkDefault true;
  hardware.trackpoint.device = lib.mkDefault "TPPS/2 Elan TrackPoint";
  #services.fprintd.enable = lib.mkDefault true;

  environment.persistence."/keep".directories = ["/var/lib/fprint"];
}
