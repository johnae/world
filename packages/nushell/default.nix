{
  lib,
  d2n,
  pkgs,
  inputs,
}:
(d2n.makeOutputs {
  inherit pkgs;
  source = inputs.nushell;
  packageOverrides."^.*".addDeps = {
    overrideAttrs = old: {
      nativeBuildInputs = (old.nativeBuildInputs or []) ++ [pkgs.pkg-config pkgs.python3];
      buildInputs = (old.buildInputs or []) ++ [pkgs.openssl pkgs.zstd pkgs.xorg.libX11];
      doCheck = false;
      buildFeatures = ["extra"];
      cargoUpdateHook = ''
        cargo add zstd-sys --features pkg-config --offline
        cargo update --package zstd-sys --offline
      '';
      shellPath = "/bin/nu";
    };
  };
})
.packages
.nu
