{ stdenv, rustPlatform, inputs }:
rustPlatform.buildRustPackage rec {
  pname = "blur";
  version = inputs.blur.rev;

  src = inputs.blur;
  cargoSha256 = "sha256-Y/fo7DvYYh7i59+LPA3iijOL5i1EzKuj+a4ESVUkKL4=";

  doCheck = false;

  meta = with stdenv.lib; {
    description = "Blurring etc for sway lock screen";
    homepage = "https://github.com/johnae/blur";
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
