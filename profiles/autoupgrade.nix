{hostName, ...}:

{
  system.autoUpgrade = {
    enable = true;
    flake = "github:johnae/world/main#${hostName}";
    allowReboot = true;
    randomizedDelaySec = "30min";
  };
}
