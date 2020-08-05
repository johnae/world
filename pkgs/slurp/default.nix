{ stdenv
, meson
, ninja
, pkgconfig
, wayland
, wayland-protocols
, cairo
, libxkbcommon
, libjpeg
, git
, systemd
, scdoc
, inputs
}:
let
  version = inputs.slurp.rev;
in
stdenv.mkDerivation {
  name = "slurp-${version}";
  inherit version;

  src = inputs.slurp;

  nativeBuildInputs = [ meson ninja pkgconfig git scdoc ];
  buildInputs = [
    wayland
    wayland-protocols
    cairo
    libjpeg
    libxkbcommon
    systemd
  ];

  meta = {
    inherit (inputs.slurp) description homepage;
    license = stdenv.lib.licenses.mit;
    platforms = stdenv.lib.platforms.linux;
  };
}
