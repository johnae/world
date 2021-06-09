{ stdenv
, lib
, pkgconfig
, meson
, ninja
, wayland
, wayland-protocols
, scdoc
, libxkbcommon
, libvncserver
, libpthreadstubs
, pixman
, aml
, libglvnd
, pam
, mesa
, neatvnc
, libX11
, libdrm
, inputs
}:

stdenv.mkDerivation {
  name = "wayvnc";
  version = inputs.wayvnc.rev;

  src = inputs.wayvnc;

  #patches = [
  #  ./disable-input.patch
  #];

  nativeBuildInputs = [ pkgconfig meson ninja scdoc ];
  buildInputs = [
    wayland
    wayland-protocols
    libxkbcommon
    libvncserver
    libpthreadstubs
    pixman
    aml
    libglvnd
    neatvnc
    libX11
    libdrm
    pam
    mesa
  ];

  enableParallelBuilding = true;

  meta = {
    license = lib.licenses.mit;
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
