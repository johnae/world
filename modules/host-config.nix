{lib, ...}: let
  inherit (lib) mkOption;
  inherit (lib.types) str nullOr attrsOf listOf enum bool;
in {
  options = {
    publicKey = mkOption {
      type = str;
      default = "xxxx";
    };
    syncthingDeviceID = mkOption {
      type = nullOr str;
      default = null;
    };
    u2fMappings = mkOption {
      type = attrsOf (listOf str);
      default = {};
    };
    wgPublicKey = mkOption {
      type = nullOr str;
      default = null;
    };
    cryptsetup.luksFormat.extraParams = mkOption {
      type = str;
      default = "-v --type luks2 --hash sha256 --key-size 256 --use-urandom --cipher aes-xts-plain64";
    };
    machinePurpose = mkOption {
      type = enum ["server" "workstation"];
      default = "server";
    };
    disk.dosLabel = mkOption {
      type = bool;
      default = false;
    };
    btrfs.format.extraParams = mkOption {
      type = str;
      default = "";
    };
    btrfs.disks = mkOption {
      type = listOf str;
      default = ["/dev/nvme0n1"];
    };
    adminUser = mkOption {
      type = str;
    };
  };
}
