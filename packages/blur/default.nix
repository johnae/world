{ lib, rustPlatform, inputs }:
rustPlatform.buildRustPackage rec {
  pname = "blur";
  version = inputs.blur.rev;

  src = inputs.blur;
  cargoSha256 = "sha256-7o90FfQ10db3FJqbVLNo1TFbZank24Tpvjpi3VbXcZQ=";

  doCheck = false;

  meta = with lib; {
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
