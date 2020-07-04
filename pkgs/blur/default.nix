{ stdenv, rustPlatform, inputs }:
rustPlatform.buildRustPackage rec {
  pname = "blur";
  version = inputs.blur.rev;

  src = inputs.blur;
  cargoSha256 = "1gi84ialj15fz6ispk245pk8ncwaw86kr2yzwzi1wqnq7gnfixv3";

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
