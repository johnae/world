{ stdenv, rustPlatform, pkgconfig, openssl, inputs }:

rustPlatform.buildRustPackage {
  pname = "spotnix";
  version = inputs.spotnix.rev;

  src = inputs.spotnix;
  cargoSha256 = "sha256-OhVg5r44Zfjr2bY3dkRg6rztFQB9oPlp6LB04ETbElw=";

  nativeBuildInputs = [ pkgconfig ];

  buildInputs = [ openssl ];

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
