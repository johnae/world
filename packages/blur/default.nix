{
  lib,
  d2n,
  pkgs,
  inputs,
}:
(d2n.makeOutputs {
  inherit pkgs;
  source = inputs.blur;
  packageOverrides."^.*".addDeps.doCheck = false;
})
.packages
.blur
