{ stdenv, rustPlatform, inputs }:
rustPlatform.buildRustPackage {
  pname = "netns-exec";
  version = inputs.netns-exec.rev;

  src = inputs.netns-exec;
  cargoSha256 = "1k757mbj9i29191vxb1aqg8yc0v4pdzsfrp97dh5446h9ism9ncj";

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
