{
  lib,
  config,
  ...
}: let
  btrfsDisks = config.btrfs.disks;
  tmpfsRootSizeGb = config.tmpfsRoot.sizegb;
in {
  fileSystems."/" = {
    device = "none";
    fsType = "tmpfs";
    options = ["defaults" "size=${toString tmpfsRootSizeGb}G" "mode=755"];
  };

  fileSystems."/nix" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = ["subvol=@nix" "rw" "noatime" "compress=zstd"];
  };

  fileSystems."/keep" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    neededForBoot = true;
    options = ["subvol=@keep" "rw" "noatime" "compress=zstd"];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/boot";
    fsType = "vfat";
  };

  swapDevices = [{device = "/dev/disk/by-label/swap";}];

  boot.initrd.supportedFilesystems = [
    "btrfs"
    "vfat"
  ];

  boot.initrd.luks.devices =
    lib.recursiveUpdate {
      cryptkey.device = "/dev/disk/by-label/cryptkey";

      encrypted_root = {
        device = "/dev/disk/by-label/encrypted_root";
        keyFile = "/dev/mapper/cryptkey";
        bypassWorkqueues = true; # https://github.com/cloudflare/linux/issues/1
        allowDiscards = true; # some security implications but not really too concerning to me
      };

      encrypted_swap = {
        device = "/dev/disk/by-label/encrypted_swap";
        keyFile = "/dev/mapper/cryptkey";
        bypassWorkqueues = true;
        allowDiscards = true;
      };
    }
    (
      builtins.listToAttrs (lib.imap1 (idx: device: {
        name = "encrypted_root${toString idx}";
        value = {
          device = "/dev/disk/by-label/encrypted_root${toString idx}";
          keyFile = "/dev/mapper/cryptkey";
          bypassWorkqueues = true;
          allowDiscards = true;
        };
      }) (builtins.tail btrfsDisks))
    );

  services.btrfs.autoScrub.enable = true;
  services.btrfs.autoScrub.fileSystems = ["/nix" "/keep"];
}
