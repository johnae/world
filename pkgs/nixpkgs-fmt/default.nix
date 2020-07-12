{ stdenv, rustPlatform, inputs }:
rustPlatform.buildRustPackage rec {
  pname = "nixpkgs-fmt";
  version = inputs.nixpkgs-fmt.rev;

  src = inputs.nixpkgs-fmt;
  cargoSha256 = "sha256-fdj0BRDIIrgdXEDGIH3/m+OC0zsL8tFwd5mOJJALe+8=";

  doCheck = false;

  meta = with stdenv.lib; {
    license = licenses.mit;
    maintainers = [
      {
        email = "john@insane.se";
        github = "johnae";
        name = "John Axel Eriksson";
      }
    ];
  };
}
