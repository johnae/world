{
  dockerTools,
  pkgs,
  lib,
  dockerRegistry ? "ghcr.io/johnae",
  dockerTag ? null,
  plainPort ? 3478,
  securePort ? 5349,
  dataPath ? "/coturn",
  workingDir ? "/coturn",
  uid ? 999,
  user ? "coturn",
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
  turn = pkgs.writeStrictShellScript "turn" ''
    export PATH=${pkgs.dig}/bin:${pkgs.coturn}/bin:$PATH
    exec turnserver \
      --log-file=stdout \
      --external-ip="$(dig @resolver1.opendns.com myip.opendns.com A -4 +short)" \
      "$@"
  '';
in
  dockerTools.buildLayeredImage {
    name = "${dockerRegistry}/coturn";
    tag = dockerTag;
    maxLayers = 6;
    config = {
      WorkingDir = workingDir;
      EntryPoint = ["${turn}"];
      ExposedPorts = {
        "${toString plainPort}/tcp" = {};
        "${toString securePort}/tcp" = {};
      };
      Volumes = {
        ${workingDir} = {};
      };
      Env = [
        "USER=${user}"
      ];
    };
    extraCommands = ''
      mkdir -p ./coturn
      chmod 777 ./coturn
    '';
    contents =
      [
        pkgs.bash
        pkgs.coreutils-full
        pkgs.cacert.out
        pkgs.coturn
        pkgs.dig
      ]
      ++ nonRootShadowSetup {
        inherit user uid;
      };
  }
