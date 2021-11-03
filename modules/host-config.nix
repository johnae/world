{lib, ...}:
let
  inherit (lib) mkOption;
  inherit (lib.types) str nullOr;
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
      default = "";
    };
  };
}
