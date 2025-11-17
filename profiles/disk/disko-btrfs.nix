{
  config,
  lib,
  ...
}: {
  disko.devices = {
    disk.disk1 = {
      device = lib.mkDefault "/dev/sda";
      type = "disk";

      content = {
        type = "gpt";
        partitions = {
          boot = {
            name = "boot";
            size = "1M";
            type = "EF02";
          };
          esp = {
            name = "ESP";
            size = "500M";
            type = "EF00";
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot";
            };
          };
          encryptedSwap = {
            size = "64G";
            content = {
              type = "swap";
              randomEncryption = true;
            };
          };
          luks = {
            size = "100%";
            content = {
              type = "luks";
              name = "encrypted";
              settings.allowDiscards = true;
              passwordFile = "/tmp/disk.key";
              content = {
                type = "btrfs";
                extraArgs = ["-f"];
                subvolumes = {
                  "/root" = lib.mkIf (!config.ephemeralRoot) {
                    mountOptions = ["compress=zstd" "noatime"];
                    mountpoint = "/";
                  };
                  "/nix" = {
                    mountOptions = ["compress=zstd" "noatime"];
                    mountpoint = "/nix";
                  };
                  "/keep" = {
                    mountOptions = ["compress=zstd" "noatime"];
                    mountpoint = "/keep";
                  };
                };
              };
            };
          };
        };
      };
    };
    nodev."/" = lib.mkIf config.ephemeralRoot {
      fsType = "tmpfs";
      mountOptions = [
        "size=16G"
        "defaults"
        "mode=755"
      ];
    };
  };
  fileSystems."/keep".neededForBoot = true;
}
