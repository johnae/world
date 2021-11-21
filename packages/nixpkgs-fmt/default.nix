{ lib, rustPlatform, inputs }:
rustPlatform.buildRustPackage {
  pname = "nixpkgs-fmt";
  version = inputs.nixpkgs-fmt.rev;

  src = inputs.nixpkgs-fmt;
  cargoSha256 = "sha256-dwkOOKDTyEt0vCLMItdC34TJWhv2A6XuMNyFlKBEDGI=";

  doCheck = false;

  meta = {
    license = lib.licenses.mit;
    maintainers = [
      {
        email = "john@insane.se";
        github = "johnae";
        name = "John Axel Eriksson";
      }
    ];
  };
}
