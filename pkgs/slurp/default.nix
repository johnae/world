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
stdenv.mkDerivation rec {
  name = "${inputs.slurp.repo}-${version}";
  version = inputs.slurp.rev;

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

  meta = with stdenv.lib; {
    inherit (inputs.slurp) description homepage;
    license = licenses.mit;
    platforms = platforms.linux;
  };
}
