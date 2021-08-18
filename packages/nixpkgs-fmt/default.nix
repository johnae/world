{ lib, rustPlatform, inputs }:
rustPlatform.buildRustPackage {
  pname = "nixpkgs-fmt";
  version = inputs.nixpkgs-fmt.rev;

  src = inputs.nixpkgs-fmt;
  cargoSha256 = "sha256-QeVn0egLcyp/EN8TtoGo6CeDz0phMdFp845Sbj8w2MY=";

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
