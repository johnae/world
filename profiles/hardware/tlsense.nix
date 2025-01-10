{lib, ...}: {
  imports = [
    ./intel.nix
  ];
  boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "ehci_pci" "sd_mod" "sdhci_pci"];

  networking.usePredictableInterfaceNames = lib.mkForce true;
}
