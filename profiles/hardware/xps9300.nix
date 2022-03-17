{
  imports = [
    ./intel.nix
  ];
  boot.kernelParams = ["mem_sleep_default=deep"];
}
