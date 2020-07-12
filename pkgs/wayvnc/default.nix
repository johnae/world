{ stdenv
, pkgconfig
, meson
, ninja
, wayland
, wayland-protocols
, libxkbcommon
, libvncserver
, libpthreadstubs
, pixman
, aml
, libglvnd
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

  nativeBuildInputs = [ pkgconfig meson ninja ];
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
    mesa
  ];

  enableParallelBuilding = true;

  meta = with stdenv.lib; {
    license = licenses.mit;
    platforms = platforms.linux;
    maintainers = [
      {
        email = "john@insane.se";
        github = "johnae";
        name = "John Axel Eriksson";
      }
    ];
  };
}
