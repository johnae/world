{
  dockerTools,
  pkgs,
  lib,
  dockerRegistry ? "ghcr.io/johnae",
  dockerTag ? null,
  port ? 6167,
  listen ? "0.0.0.0",
  dbPath ? "/data",
  workingDir ? "/conduit",
  user ? "conduit",
}: let
  nonRootShadowSetup = {
    user,
    uid,
    gid ? uid,
  }: [
    (
      pkgs.writeTextDir "etc/shadow" ''
        root:!x:::::::
        ${user}:!:::::::
      ''
    )
    (
      pkgs.writeTextDir "etc/passwd" ''
        root:x:0:0::/root:${pkgs.runtimeShell}
        ${user}:x:${toString uid}:${toString gid}::/home/${user}:
      ''
    )
    (
      pkgs.writeTextDir "etc/group" ''
        root:x:0:
        ${user}:x:${toString gid}:
      ''
    )
    (
      pkgs.writeTextDir "etc/gshadow" ''
        root:x::
        ${user}:x::
      ''
    )
  ];
in
  dockerTools.buildLayeredImage {
    name = "${dockerRegistry}/matrix-conduit";
    tag = dockerTag;
    maxLayers = 6;
    config = {
      WorkingDir = "/conduit";
      EntryPoint = ["${pkgs.matrix-conduit}/bin/conduit"];
      ExposedPorts = {
        "${toString port}/tcp" = {};
      };
      Env = [
        "RUST_BACKTRACE=1"
        "CONDUIT_PORT=${toString port}"
        "CONDUIT_ADDRESS=${listen}"
        "CONDUIT_DATABASE_PATH=${dbPath}"
        "CONDUIT_CONFIG=''" ## only use env vars
        "USER=${user}"
      ];
      Volumes = {
        ${dbPath} = {};
      };
    };
    extraCommands = ''
      mkdir -p ./conduit ./data
      chmod 777 ./conduit ./data
    '';
    contents =
      [
        pkgs.bash
        pkgs.coreutils-full
        pkgs.cacert.out
        pkgs.matrix-conduit
      ]
      ++ nonRootShadowSetup {
        uid = 999;
        inherit user;
      };
  }
