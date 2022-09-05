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
    nativeBuildInputs = old: old ++ [pkgs.rustPlatform.bindgenHook pkgs.pkgconfig];
    buildInputs = old: old ++ [pkgs.rocksdb];
    doCheck = false;
    cargoBuildFlags = "--no-default-features --features conduit_bin,backend_sqlite,backend_rocksdb";
  };
})
.packages
.conduit
