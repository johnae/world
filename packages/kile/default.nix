{
  lib,
  rustPlatform,
  inputs,
}:
rustPlatform.buildRustPackage rec {
  pname = "kile";
  version = inputs.kile.rev;

  src = inputs.kile;
  cargoSha256 = "sha256-Np8ATTSZTnAsPvTcViWlK8g9dUIxWqbSJ3YgTYdapeI=";

  doCheck = false;

  meta = with lib; {
    description = "River layout generator";
    homepage = "https://gitlab.com/snakedye/kile";
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
