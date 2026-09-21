{
  lib,
  beautifulsoup4,
  buildPythonPackage,
  certifi,
  fetchPypi,
  pycryptodome,
  requests,
  setuptools,
  tzdata,
}: let
  version = "4.32.0";
in
  buildPythonPackage {
    pname = "hyundai-kia-connect-api";
    inherit version;
    pyproject = true;

    src = fetchPypi {
      pname = "hyundai_kia_connect_api";
      inherit version;
      hash = "sha256-IbfRHClERLnjuvpWvTX9SuJod/PRNDfVK99vGAcqxbk=";
    };

    build-system = [setuptools];

    ## upstream wants certifi>=2026.7.22, nixpkgs carries 2026.06.17
    pythonRelaxDeps = ["certifi"];

    dependencies = [
      beautifulsoup4
      certifi
      pycryptodome
      requests
      tzdata
    ];

    pythonImportsCheck = ["hyundai_kia_connect_api"];

    meta = {
      description = "Python client for the Hyundai Bluelink and Kia Connect APIs";
      homepage = "https://github.com/Hyundai-Kia-Connect/hyundai_kia_connect_api";
      license = lib.licenses.mit;
    };
  }
