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
    wgPublicKey = mkOption {
      type = nullOr str;
      default = null;
    };
  };
}
