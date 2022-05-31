{
  dockerTools,
  pkgs,
  lib,
  dockerRegistry ? "ghcr.io/0b2ce",
  dockerTag ? null,
  ...
}:
dockerTools.buildLayeredImage {
  name = "${dockerRegistry}/coreutils";
  tag = dockerTag;
  maxLayers = 6;
  config.Entrypoint = ["${pkgs.bash}/bin/bash"];
  contents = [
    pkgs.bash
    pkgs.coreutils-full
    pkgs.cacert.out
  ];
}
