{ stdenv, lib, rustPlatform, inputs }:

with rustPlatform;
buildRustPackage rec {
  pname = "persway";
  version = inputs.persway.rev;

  src = inputs.persway;

  cargoSha256 = "03p9gl9l4p6cj7jggdiiv00pfpmm88zwj2vc39a6ccrv46hqq5qf";

  outputs = [ "out" ];

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
