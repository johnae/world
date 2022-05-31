{
  dockerTools,
  pkgs,
  lib,
  dockerRegistry ? "ghcr.io/johnae",
  dockerTag ? null,
  ...
}:
dockerTools.buildLayeredImage {
  name = "${dockerRegistry}/sqlite";
  tag = dockerTag;
  maxLayers = 6;
  config.Entrypoint = ["${pkgs.bash}/bin/bash"];
  contents = [
    pkgs.bash
    pkgs.sqlite
    pkgs.coreutils-full
    pkgs.cacert.out
  ];
}
