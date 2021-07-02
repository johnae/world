{...}:

{
  system.autoUpgrade = {
    enable = true;
    flake = "github:johnae/world/main";
    allowReboot = true;
    randomizedDelaySec = "30min";
  };
}
