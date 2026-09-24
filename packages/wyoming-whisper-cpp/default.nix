## whisper.cpp serves whisper over HTTP on the GPU; Home Assistant speaks
## wyoming. This bridges the two and builds the biasing prompt from Home
## Assistant's names with wyoming-faster-whisper's own name cache, so the GPU
## server gets the same prompt the CPU one would.
{
  lib,
  python3Packages,
  wyoming-faster-whisper,
  ...
}:
python3Packages.buildPythonApplication {
  pname = "wyoming-whisper-cpp";
  version = "0.1.0";
  format = "other";

  src = ./.;

  propagatedBuildInputs = with python3Packages; [
    aiohttp
    huggingface-hub
    tokenizers
    wyoming
    ## An application rather than a library in nixpkgs, but its name cache and
    ## Home Assistant client are plain modules that only need aiohttp.
    (toPythonModule wyoming-faster-whisper)
  ];

  dontBuild = true;

  installPhase = ''
    runHook preInstall
    install -Dm755 wyoming_whisper_cpp.py $out/bin/wyoming-whisper-cpp
    runHook postInstall
  '';

  meta = {
    description = "Wyoming speech-to-text bridge to whisper.cpp's whisper-server";
    mainProgram = "wyoming-whisper-cpp";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
  };
}
