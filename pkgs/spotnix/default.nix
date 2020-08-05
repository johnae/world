{ stdenv, rustPlatform, pkgconfig, openssl, inputs }:

rustPlatform.buildRustPackage {
  pname = "spotnix";
  version = inputs.spotnix.rev;

  src = inputs.spotnix;
  cargoSha256 = "0p0jvd2f0x5hx1lzk83x00ayvg7ac127cdxnv7mzhr9qpvk6059s";

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
