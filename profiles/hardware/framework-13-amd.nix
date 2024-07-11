{
  inputs,
  lib,
  ...
}: {
  imports = [
    inputs.nixos-hardware.nixosModules.framework-13-7040-amd
  ];
  boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "thunderbolt" "usb_storage" "sd_mod"];
  services.tlp.enable = lib.mkForce false;
  environment.persistence."/keep" = {
    directories = ["/var/lib/fprint"];
  };
}
