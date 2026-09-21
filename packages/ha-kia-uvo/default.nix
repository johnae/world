## Not in nixpkgs. Gives state of charge, plug state and climate
## preconditioning for the EV9. Kia Connect rate-limits per account, so keep
## the poll interval at the integration default or higher.
{
  lib,
  buildHomeAssistantComponent,
  fetchFromGitHub,
  home-assistant,
  ...
}: let
  version = "3.14.0";
  hyundai-kia-connect-api = home-assistant.python3Packages.callPackage ./hyundai-kia-connect-api.nix {};
in
  buildHomeAssistantComponent {
    owner = "Hyundai-Kia-Connect";
    domain = "kia_uvo";
    inherit version;

    src = fetchFromGitHub {
      owner = "Hyundai-Kia-Connect";
      repo = "kia_uvo";
      tag = "v${version}";
      hash = "sha256-um5EIR9L3Cksfd5vEHUZihQVnkk5+gYvfd3XWZNdRl4=";
    };

    dependencies = [hyundai-kia-connect-api];

    meta = {
      description = "Kia Connect and Hyundai Bluelink integration for Home Assistant";
      homepage = "https://github.com/Hyundai-Kia-Connect/kia_uvo";
      changelog = "https://github.com/Hyundai-Kia-Connect/kia_uvo/releases/tag/v${version}";
      license = lib.licenses.mit;
    };
  }
