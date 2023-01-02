{
  lib,
  d2n,
  pkgs,
  inputs,
}:
(d2n.makeOutputs {
  inherit pkgs;
  source = inputs.matrix-conduit;
  packageOverrides."^.*".addDeps = {
    overrideAttrs = old: {
      nativeBuildInputs = (old.nativeBuildInputs or []) ++ [pkgs.rustPlatform.bindgenHook pkgs.pkg-config];
      buildInputs = (old.buildInputs or []) ++ [pkgs.rocksdb];
      doCheck = false;
      cargoBuildFlags = "--no-default-features --features conduit_bin,backend_sqlite,backend_rocksdb";
    };
  };
})
.packages
.conduit
