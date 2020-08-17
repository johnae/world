{ stdenv, rustPlatform, inputs }:
rustPlatform.buildRustPackage {
  pname = "nixpkgs-fmt";
  version = inputs.nixpkgs-fmt.rev;

  src = inputs.nixpkgs-fmt;
  cargoSha256 = "sha256-8+F0LZGAWrl3wtSQnDI4PA8L587vHX7yIZozNMUEXC0=";

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
