{lib, ...}:
let
  inherit (lib) mkOption;
  inherit (lib.types) str nullOr listOf;
in
{
  options = {
    publicKey = mkOption {
      type = str;
      default = "xxxx";
    };
    syncthingDeviceID = mkOption {
      type = nullOr str;
      default = null;
    };
    wgPublicKey = mkOption {
      type = nullOr str;
      default = null;
    };
    cryptsetup.luksFormat.extraParams = mkOption {
      type = str;
      default = "-v --type luks2 --sector-size 4096 --hash sha256 --key-size 256 --use-urandom --cipher aes-xts-plain64 --align-payload=2048";
    };
    btrfs.format.extraParams = mkOption {
      type = str;
      default = "-s 4096";
    };
    btrfs.disks = mkOption {
      type = listOf str;
      default = [ "/dev/nvme0n1" ];
    };
  };
}
