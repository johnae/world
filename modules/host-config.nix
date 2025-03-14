{lib, ...}: let
  inherit (lib) mkOption;
  inherit (lib.types) str nullOr attrsOf listOf enum bool ints attrs strMatching;
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
      default = "-v";
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
    btrfs.subvolumes = mkOption {
      type = listOf str;
      default = ["nix" "home" "var"];
    };
    bcachefs.disks = mkOption {
      type = listOf str;
      default = [];
    };
    bcachefs.devices = mkOption {
      type = listOf str;
      default = [];
    };
    bcachefs.subvolumes = mkOption {
      type = listOf str;
      default = ["nix" "home" "var"];
    };
    tmpfsRoot.sizegb = mkOption {
      type = ints.between 2 64;
      default = 8;
    };
    tmpfsRoot.size = mkOption {
      type = strMatching "^([0-9]{1,2}|100)%$";
      default = "50%";
    };
    adminUser = mkOption {
      type = attrs;
      default = {};
    };
  };
}
