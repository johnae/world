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
    nativeBuildInputs = old: old ++ [pkgs.pkgconfig];
    buildInputs = old: old ++ [pkgs.pcsclite];
    doCheck = false;
  };
})
.packages
.age-plugin-yubikey
