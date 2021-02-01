{ stdenv
, lib
, fetchFromGitHub
, meson
, ninja
, pkgconfig
, inputs
}:
stdenv.mkDerivation {
  name = "aml";
  version = inputs.aml.rev;

  src = inputs.aml;

  nativeBuildInputs = [ meson ninja pkgconfig ];

  buildInputs = [ ];

  meta = {
    description = "Andri's Main Loop";
    homepage = "https://github.com/any1/aml";
    license = lib.licenses.isc;
    platforms = lib.platforms.linux;
  };
}
