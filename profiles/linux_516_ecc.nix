{
  pkgs,
  lib,
  ...
}: {
  boot.kernelPackages = lib.mkForce pkgs.linuxPackages_5_16_eccPatch;
}
