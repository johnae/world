## Supertonic has no server of its own, so Home Assistant cannot reach it
## without something that speaks wyoming in front.
{
  lib,
  python3Packages,
  supertonic,
  ...
}:
python3Packages.buildPythonApplication {
  pname = "wyoming-supertonic";
  version = "0.1.0";
  format = "other";

  src = ./.;

  propagatedBuildInputs = with python3Packages;
    [
      numpy
      wyoming
    ]
    ++ [supertonic];

  dontBuild = true;

  installPhase = ''
    runHook preInstall
    install -Dm755 wyoming_supertonic.py $out/bin/wyoming-supertonic
    runHook postInstall
  '';

  meta = {
    description = "Wyoming text-to-speech server backed by Supertonic 3";
    mainProgram = "wyoming-supertonic";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
  };
}
