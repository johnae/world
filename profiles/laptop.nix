{ hostName, config, lib, pkgs, inputs, ... }:
{
  imports = [
    ./desktop.nix
  ];

  services.logind.lidSwitch = "suspend-then-hibernate";
  services.disable-usb-wakeup.enable = true;
}
