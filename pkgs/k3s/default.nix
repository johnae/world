{ stdenv, lib }:
let
  metadata = import ./metadata.nix;
in
stdenv.mkDerivation {
  inherit (metadata) version;
  name = "k3s";
  src = builtins.fetchurl { inherit (metadata) url sha256; };

  installPhase = ''
    mkdir -p $out/bin
    cp $src $out/bin/k3s
    chmod +x $out/bin/k3s
  '';

  unpackPhase = "true";
  buildPhase = "true";

  meta = {
    description = "Lightweight Kubernetes";
    longDescription = ''
      Lightweight Kubernetes is a fully certified kubernetes but
      much simpler to deploy and manage while using less reinputs.
    '';
    homepage = "https://github.com/rancher/k3s";
    platforms = lib.platforms.linux;
  };
}
