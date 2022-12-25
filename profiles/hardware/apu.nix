{
  boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "ehci_pci" "sd_mod" "sdhci_pci"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = ["kvm-amd"];
  boot.extraModulePackages = [];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  boot.kernelParams = [
    "console=ttyS0,115200"
    "console=tty1"
  ];

  hardware.cpu.amd.updateMicrocode = true;
}
