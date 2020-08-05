{ stdenv, lib, rustPlatform, inputs }:

rustPlatform.buildRustPackage {
  pname = "persway";
  version = inputs.persway.rev;

  src = inputs.persway;

  cargoSha256 = "sha256-NiYQGDyIYE2NjWFoZcGrTtq/vTzuA/iWDJNtbT9qKgU=";

  outputs = [ "out" ];

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
