{lib, ...}: {
  boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "ehci_pci" "sd_mod" "sdhci_pci"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = ["kvm-amd"];
  boot.extraModulePackages = [];

  networking.usePredictableInterfaceNames = lib.mkForce true;

  boot.loader.systemd-boot.enable = lib.mkForce false;
  boot.loader.efi.canTouchEfiVariables = lib.mkForce false;

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  boot.kernelParams = ["console=ttyS0,115200n8"];

  boot.loader.grub.extraConfig = "
    serial --speed=115200 --unit=0 --word=8 --parity=no --stop=1
    terminal_input serial
    terminal_output serial
  ";

  hardware.cpu.amd.updateMicrocode = true;

  boot.initrd.luks.devices.cryptkey.keyFile = "/sys/devices/virtual/dmi/id/product_serial";
}
