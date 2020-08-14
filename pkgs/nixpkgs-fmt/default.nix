{ stdenv, rustPlatform, inputs }:
rustPlatform.buildRustPackage {
  pname = "nixpkgs-fmt";
  version = inputs.nixpkgs-fmt.rev;

  src = inputs.nixpkgs-fmt;
  cargoSha256 = "sha256-m5VT09iz2/RITzzEpA0mc652/KhTiSt3gJ6IFpKWu7Q=";

  doCheck = false;

  meta = {
    license = stdenv.lib.licenses.mit;
    maintainers = [
      {
        email = "john@insane.se";
        github = "johnae";
        name = "John Axel Eriksson";
      }
    ];
  };
}
