{
  lib,
  d2n,
  pkgs,
  inputs,
}:
(d2n.makeOutputs {
  inherit pkgs;
  source = inputs.netns-exec;
  packageOverrides."^.*".addDeps.doCheck = false;
})
.packages
.netns-exec
