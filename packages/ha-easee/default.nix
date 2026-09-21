## Not in nixpkgs; the Easee cloud API is the only way to read the charger,
## since Easee exposes nothing on the LAN. Read-only here - Tibber owns
## charging control.
{
  lib,
  buildHomeAssistantComponent,
  fetchFromGitHub,
  home-assistant,
  ...
}: let
  version = "0.9.74";
  pyeasee = home-assistant.python3Packages.callPackage ./pyeasee.nix {};
in
  buildHomeAssistantComponent {
    owner = "nordicopen";
    domain = "easee";
    inherit version;

    src = fetchFromGitHub {
      owner = "nordicopen";
      repo = "easee_hass";
      tag = "v${version}";
      hash = "sha256-PphFsSqJKRhA73rnh0Wfn0ntrKL9ZIvjZpHERJ2wMLQ=";
    };

    ## the repo ships a release Makefile that stdenv would otherwise run
    dontBuild = true;

    dependencies = [pyeasee];

    meta = {
      description = "Easee EV charger integration for Home Assistant";
      homepage = "https://github.com/nordicopen/easee_hass";
      changelog = "https://github.com/nordicopen/easee_hass/releases/tag/v${version}";
      license = lib.licenses.mit;
    };
  }
