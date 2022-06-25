{
  lib,
  rustPlatform,
  fetchFromGitLab,
  inputs,
}:
rustPlatform.buildRustPackage rec {
  pname = "ristate";
  version = inputs.ristate.rev;

  src = inputs.ristate;

  cargoSha256 = "sha256-cfnYIoQmqJaIV33kMd8XgsSOVZf5O8E3Jpm8gAFXOM0=";

  doCheck = false;

  meta = with lib; {
    description = "A river-status client written in Rust";
    homepage = "https://gitlab.com/snakedye/ristate";
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
