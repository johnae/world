{ lib, rustPlatform, inputs }:
rustPlatform.buildRustPackage {
  pname = "netns-exec";
  version = inputs.netns-exec.rev;

  src = inputs.netns-exec;
  cargoSha256 = "sha256-a10IAp+PGnJAmLJIkKlRKI3a82SbV1jj5jEB64DZr6o=";

  doCheck = false;

  meta = {
    description = "Execute process within Linux network namespace";
    homepage = "https://github.com/johnae/netns-exec";
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
