{
  lib,
  d2n,
  pkgs,
  inputs,
}:
(d2n.makeOutputs {
  inherit pkgs;
  source = inputs.kile;
  packageOverrides."^.*".addDeps.doCheck = false;
})
.packages
.kile
