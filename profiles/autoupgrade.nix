{hostName, ...}:

{
  system.autoUpgrade = {
    enable = true;
    flake = "github:johnae/world#${hostName}";
    allowReboot = true;
    randomizedDelaySec = "30min";
  };
}
