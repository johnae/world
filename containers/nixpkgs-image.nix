{ pkgs }:
let
  meta = import ./nixpkgs-image-meta.nix;
in
pkgs.dockerTools.pullImage meta
