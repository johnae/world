{
  lib,
  config,
  ...
}: let
  bcacheFsDisks = config.bcachefs.disks;
  tmpfsRootSizeGb = config.tmpfsRoot.sizegb;
in {
  fileSystems."/" = {
    device = "none";
    fsType = "tmpfs";
    options = ["defaults" "size=${toString tmpfsRootSizeGb}G" "mode=755"];
  };

  fileSystems."/keep" = {
    device = lib.concatStringsSep ":" bcacheFsDisks;
    fsType = "bcachefs";
    options = ["defaults" "compression=zstd"];
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

  swapDevices = [
    {
      label = "swap";
      encrypted = {
        enable = true;
        label = "encrypted_swap";
        keyfile = "/sysroot/keep/encrypted_swap.key";
      };
    }
  ];
}
