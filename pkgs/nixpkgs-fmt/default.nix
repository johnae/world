{ stdenv, rustPlatform, inputs }:
rustPlatform.buildRustPackage rec {
  pname = "nixpkgs-fmt";
  version = inputs.nixpkgs-fmt.rev;

  src = inputs.nixpkgs-fmt;
  cargoSha256 = "0p86gq1kmngy9yr7hpqd0vhjv2s7jl81vacffz8si5w8i79zrzy5";

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
