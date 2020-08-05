{ stdenv
, coreutils
, gnused
, meson
, ninja
, pkgconfig
, wayland
, wayland-protocols
, git
, systemd
, inputs
}:

stdenv.mkDerivation {
  name = "wl-clipboard";
  version = inputs.wl-clipboard.rev;

  src = inputs.wl-clipboard;

  preConfigure = ''
    echo "Fixing cat path..."
    ${gnused}/bin/sed -i"" 's|\(/bin/cat\)|${coreutils}\1|g' src/wl-paste.c
  '';

  mesonFlags = [
    "-Dfishcompletiondir=no"
  ];

  nativeBuildInputs = [ meson ninja pkgconfig git ];
  buildInputs = [ wayland wayland-protocols ];

  enableParallelBuilding = true;

  meta = {
    license = stdenv.lib.licenses.gpl3;
    platforms = stdenv.lib.platforms.linux;
  };
}
