{ stdenv, rustPlatform, inputs }:
rustPlatform.buildRustPackage {
  pname = "nixpkgs-fmt";
  version = inputs.nixpkgs-fmt.rev;

  src = inputs.nixpkgs-fmt;
  cargoSha256 = "sha256-WMDYD9UIlWzSvAEZ2hddPd8lQ+YTDYyNiRV3zukdZu8=";

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
