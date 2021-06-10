{...}:

{
  services.udev.extraRules = ''
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", MODE="0664", GROUP="plugdev", ATTRS{idVendor}=="1050"
  '';
}
