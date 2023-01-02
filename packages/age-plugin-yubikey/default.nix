{
  lib,
  d2n,
  pkgs,
  inputs,
}:
(d2n.makeOutputs {
  inherit pkgs;
  source = inputs.age-plugin-yubikey;
  packageOverrides."^.*".addDeps = {
    overrideAttrs = old: {
      nativeBuildInputs = (old.nativeBuildInputs or []) ++ [pkgs.pkg-config];
      buildInputs = (old.buildInputs or []) ++ [pkgs.pcsclite];
      doCheck = false;
    };
  };
})
.packages
.age-plugin-yubikey
