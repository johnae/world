{
  lib,
  aiohttp,
  buildPythonPackage,
  fetchPypi,
  pysignalr,
  setuptools,
}: let
  version = "0.8.17";
in
  buildPythonPackage {
    pname = "pyeasee";
    inherit version;
    pyproject = true;

    src = fetchPypi {
      pname = "pyeasee";
      inherit version;
      hash = "sha256-TDvy2zQ65Pa20fp/p23tJDCRjbJLs6gd5JIx4Cv5j9s=";
    };

    build-system = [setuptools];

    ## upstream pins pysignalr==1.3.0, nixpkgs carries 1.3.2
    pythonRelaxDeps = ["pysignalr"];

    dependencies = [
      aiohttp
      pysignalr
    ];

    pythonImportsCheck = ["pyeasee"];

    meta = {
      description = "Easee EV charger API library";
      homepage = "https://github.com/nordicopen/pyeasee";
      license = lib.licenses.mit;
    };
  }
