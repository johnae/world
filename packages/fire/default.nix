{
  stdenv,
  ghc,
  inputs,
}:
stdenv.mkDerivation rec {
  version = inputs.fire.rev;
  name = "fire-${version}";

  src = inputs.fire;

  buildPhase = ''
    ghc -O2 fire.hs
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    cp fire $out/bin/
    runHook postInstall
  '';

  buildInputs = [ghc];

  meta = {
    description = "Simple launcher (creates new process group for exec'd process)";
    homepage = "https://github.com/johnae/fire";
    license = "MIT";
  };
}
