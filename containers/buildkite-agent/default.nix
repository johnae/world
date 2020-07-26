{ stdenv
, writeText
, dockerTools
, nix-plugins
, cacert
, iana-etc
, buildkite
, bashInteractive
, openssh
, coreutils
, git
, gnutar
, gzip
, xz
, tini
, pkgs
, dockerRegistry ? "johnae"
, dockerTag ? "latest"
}:
let
  nixconf = writeText "nix.conf" ''
    sandbox = false
    experimental-features = nix-command flakes ca-references
    ## below allows the use of builtins.exec - for secrets decryption
    allow-unsafe-native-code-during-evaluation = true
  '';

  rootfs = stdenv.mkDerivation {
    version = "1";
    name = "rootfs-buildkite";
    buildCommand = ''
      mkdir -p $out/{root,etc/nix,tmp}
      cp ${nixconf} $out/etc/nix/nix.conf
    '';
  };

  nixImage = import ../nixpkgs-image.nix { inherit pkgs; };
in
dockerTools.buildImage {
  name = "${dockerRegistry}/buildkite-agent";
  tag = dockerTag;
  fromImage = nixImage;
  contents = [
    cacert
    iana-etc
    buildkite
    bashInteractive
    openssh
    coreutils
    git
    gnutar
    gzip
    xz
    tini
    rootfs
  ];

  extraCommands = ''
    mkdir -p usr/bin
    mkdir -p var/lib/buildkite/builds
    mkdir -p var/tmp
    ln -s ${coreutils}/bin/env usr/bin/env
  '';

  config = {
    Entrypoint = [
      "${tini}/bin/tini"
      "-g"
      "--"
      "${buildkite}/bin/buildkite-agent"
    ];
    Cmd = [ "start" ];
    WorkingDir = "/var/lib/buildkite/builds";
    Env = [
      "ENV=/etc/profile.d/nix.sh"
      "NIX_PATH=nixpkgs=${pkgs.path}"
      "PAGER=cat"
      "PATH=/nix/var/nix/profiles/default/bin:/usr/bin:/bin"
      "GIT_SSL_CAINFO=/etc/ssl/certs/ca-bundle.crt"
      "NIX_SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt"
      "BUILDKITE_PLUGINS_PATH=/var/lib/buildkite/plugins"
      "BUILDKITE_BUILD_PATH=/var/lib/buildkite/builds"
    ];
    Volumes = {
      "/nix" = { };
    };
  };
}
