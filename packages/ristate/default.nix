{
  lib,
  d2n,
  pkgs,
  inputs,
}:
(d2n.makeOutputs {
  inherit pkgs;
  source = inputs.ristate;
  packageOverrides."^.*".addDeps.doCheck = false;
})
.packages
.ristate
