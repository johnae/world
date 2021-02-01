{ stdenv
, lib
, fetchFromGitHub
, meson
, pkg-config
, ninja
, pixman
, aml
, gnutls
, libdrm
, libjpeg_turbo
, zlib
, inputs
}:
let
  version = inputs.neatvnc.rev;
in
stdenv.mkDerivation {
  pname = "neatvnc-${version}";
  inherit version;

  src = inputs.neatvnc;

  nativeBuildInputs = [ meson pkg-config ninja ];
  buildInputs = [
    pixman
    aml
    gnutls
    libdrm
    libjpeg_turbo
    zlib
  ];

  meta = {
    description = "A VNC server library";
    longDescription = ''
      This is a liberally licensed VNC server library that's intended to be
      fast and neat. Goals:
      - Speed
      - Clean interface
      - Interoperability with the Freedesktop.org ecosystem
    '';
    inherit (inputs.neatvnc) homepage;
    license = lib.licenses.isc;
    platforms = lib.platforms.linux;
    maintainers = [
      {
        email = "john@insane.se";
        github = "johnae";
        name = "John Axel Eriksson";
      }
    ];
  };
}
