{
  lib,
  config,
  ...
}: let
  bcacheFsDevices = config.bcachefs.devices;
  tmpfsRootSizeGb = config.tmpfsRoot.sizegb;
in {
  fileSystems."/" = {
    device = "none";
    fsType = "tmpfs";
    options = ["defaults" "size=${toString tmpfsRootSizeGb}G" "mode=755"];
  };

  fileSystems."/keep" = {
    device = lib.concatStringsSep ":" bcacheFsDevices;
    fsType = "bcachefs";
    options = ["defaults" "compression=zstd" "background_compression=zstd"];
    neededForBoot = true;
  };

  fileSystems."/nix" = {
    device = "/keep/nix";
    options = ["bind"];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/boot";
    fsType = "vfat";
  };

  boot.initrd.supportedFilesystems = [
    "bcachefs"
    "vfat"
  ];

  swapDevices = [
    {
      label = "swap";
      device = "/dev/disk/by-label/swap";
      encrypted = {
        enable = true;
        label = "encrypted_swap";
        keyFile = "/sysroot/keep/encrypted_swap.key";
        blkDev = "/dev/disk/by-label/encrypted_swap";
      };
    }
  ];
}
