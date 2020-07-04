{ stdenv
, fetchFromGitHub
, meson
, ninja
, pkgconfig
, inputs
}:
stdenv.mkDerivation rec {
  name = "aml";
  version = inputs.aml.rev;

  src = inputs.aml;

  nativeBuildInputs = [ meson ninja pkgconfig ];

  buildInputs = [ ];

  meta = with stdenv.lib; {
    description = "Andri's Main Loop";
    homepage = "https://github.com/any1/aml";
    license = licenses.isc;
    platforms = platforms.linux;
  };
}
