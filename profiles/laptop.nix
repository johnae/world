{lib, ...}: {
  imports = [
    ./workstation.nix
  ];

  services.logind.lidSwitch = "suspend-then-hibernate";
  services.disable-usb-wakeup.enable = true;
  programs.light.enable = true;
  services.upower.enable = true;
  services.tlp.enable = true;
  services.tlp.settings = {
    USB_AUTOSUSPEND = 0;
  };

  boot.kernel.sysctl = {
    "vm.dirty_writeback_centisecs" = lib.mkDefault 1500;
    "vm.laptop_mode" = lib.mkDefault 5;
    "vm.swappiness" = lib.mkDefault 1;
  };
}
