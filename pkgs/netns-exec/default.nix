{ stdenv, rustPlatform, inputs }:
rustPlatform.buildRustPackage {
  pname = "netns-exec";
  version = inputs.netns-exec.rev;

  src = inputs.netns-exec;
  cargoSha256 = "sha256-5067DMBkMU+AXAH22SmqEdELk4A+wrXcZDyBtVXhh2Q=";

  doCheck = false;

  meta = {
    description = "Execute process within Linux network namespace";
    homepage = "https://github.com/johnae/netns-exec";
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
