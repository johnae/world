{ lib
, rustPlatform
, pkgconfig
, pcsclite
, inputs
}:

rustPlatform.buildRustPackage {
  pname = "age-plugin-yubikey";
  version = inputs.age-plugin-yubikey.rev;

  src = inputs.age-plugin-yubikey;
  cargoSha256 = "sha256-mbzd/iwtquUG4OeX4vx5PH3GBdpaXX3NUUo+siCdmQU=";

  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ pcsclite ];

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
