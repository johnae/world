{ stdenv, lib, pkgs, ... }:
rec {
  insane-lib = pkgs.callPackage ./lib {};
  buildkite-pipeline = pkgs.callPackage ./buildkite-pipeline {};
  buildkite = pkgs.callPackage ./buildkite {};
}
