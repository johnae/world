## Not in nixpkgs. Supertone's on-device TTS: four small ONNX graphs rather
## than an autoregressive model, which is why it synthesises Swedish at about
## a fifth of realtime on a CPU with no GPU involved.
{
  lib,
  python3Packages,
  ...
}:
python3Packages.buildPythonPackage rec {
  pname = "supertonic";
  version = "1.3.1";
  pyproject = true;

  src = python3Packages.fetchPypi {
    inherit pname version;
    hash = "sha256-Q2fo9hr+phjayUj2vuVf7UchrWbKLT/JB3GipmdAcx4=";
  };

  build-system = [python3Packages.setuptools];

  dependencies = with python3Packages; [
    huggingface-hub
    numpy
    onnxruntime
    soundfile
  ];

  ## Everything it can test needs the model weights from Hugging Face.
  doCheck = false;

  pythonImportsCheck = ["supertonic"];

  meta = {
    description = "On-device text-to-speech across 31 languages, ONNX Runtime only";
    homepage = "https://huggingface.co/Supertone/supertonic-3";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
  };
}
