{hostName, lib, ...}:

{
  system.autoUpgrade = {
    enable = true;
    flake = lib.mkDefault "github:johnae/world#${hostName}";
    allowReboot = lib.mkDefault true;
    randomizedDelaySec = lib.mkDefault "30min";
  };
}
