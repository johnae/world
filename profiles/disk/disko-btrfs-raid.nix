{
  config,
  lib,
  ...
}: {
  disko.devices = {
    disk = {
      # Devices will be mounted and formatted in alphabetical order, and btrfs can only mount raids
      # when all devices are present. So we define an "empty" luks device on the first disk,
      # and the actual btrfs raid on the second disk, and the name of these entries matters!
      disk1 = {
        type = "disk";
        device = lib.mkDefault "/dev/sda";
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              size = "512M";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
                mountOptions = ["umask=0077"];
              };
            };
            encryptedSwap = {
              size = "64G";
              content = {
                type = "swap";
                randomEncryption = true;
              };
            };
            luks1 = {
              size = "100%";
              content = {
                type = "luks";
                name = "encrypted1";
                settings.allowDiscards = true;
                passwordFile = "/tmp/disk.key";
              };
            };
          };
        };
      };
      disk2 = {
        type = "disk";
        device = lib.mkDefault "/dev/sdb";
        content = {
          type = "gpt";
          partitions = {
            luks2 = {
              size = "100%";
              content = {
                type = "luks";
                name = "encrypted2";
                settings.allowDiscards = true;
                passwordFile = "/tmp/disk.key";
                content = {
                  type = "btrfs";
                  extraArgs = [
                    "-f"
                    "-d raid1"
                    "/dev/mapper/encrypted1"
                  ];
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
