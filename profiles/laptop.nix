{ hostName, config, lib, pkgs, inputs, ... }:
{
  imports = [
    ./workstation.nix
  ];

  services.logind.lidSwitch = "suspend-then-hibernate";
  services.disable-usb-wakeup.enable = true;
  programs.light.enable = true;
  services.upower.enable = true;

  boot.kernel.sysctl = {
    "vm.dirty_writeback_centisecs" = 1500;
    "vm.laptop_mode" = 5;
    "vm.swappiness" = 1;
  };
}
