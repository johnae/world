{ stdenv, fetchgit, wget, perl, cacert }:
let
  metadata = builtins.fromJSON (builtins.readFile ./metadata.json);
in
stdenv.mkDerivation rec {
  version = metadata.rev;
  name = "spook-${version}";
  SPOOK_VERSION = version;

  src = fetchgit {
    inherit (metadata) url rev sha256 fetchSubmodules;
  };

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    make install PREFIX=$out
    runHook postInstall
  '';

  buildInputs = [ wget perl cacert ];

  meta = {
    description =
      "Lightweight evented utility for monitoring file changes and more";
    homepage = "https://github.com/johnae/spook";
    license = "MIT";
  };

}
