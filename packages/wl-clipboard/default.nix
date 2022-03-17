{
  stdenv,
  lib,
  coreutils,
  gnused,
  meson,
  ninja,
  pkgconfig,
  wayland,
  wayland-protocols,
  git,
  inputs,
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

  nativeBuildInputs = [meson ninja pkgconfig git];
  buildInputs = [wayland wayland-protocols];

  enableParallelBuilding = true;

  meta = {
    license = lib.licenses.gpl3;
    platforms = lib.platforms.linux;
  };
}
