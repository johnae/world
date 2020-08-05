{ stdenv
, fetchpatch
, meson
, ninja
, pkgconfig
, wayland
, libGL
, wayland-protocols
, libinput
, libxkbcommon
, pixman
, xcbutilwm
, libX11
, libcap
, xcbutilimage
, xcbutilerrors
, mesa_noglu
, libglvnd
, libpng
, ffmpeg
, inputs
}:

stdenv.mkDerivation {
  name = "wlroots-${inputs.wlroots.rev}";
  version = inputs.wlroots.rev;

  src = inputs.wlroots;

  outputs = [ "out" ];

  nativeBuildInputs = [ meson ninja pkgconfig ];

  mesonFlags = [
    "-Dlibcap=enabled"
    "-Dlogind-provider=systemd"
    "-Dxwayland=enabled"
    "-Dx11-backend=enabled"
    "-Dxcb-icccm=enabled"
    "-Dxcb-errors=enabled"
    "-Dxcb-xkb=enabled"
  ];

  buildInputs = [
    wayland
    libGL
    wayland-protocols
    libinput
    libxkbcommon
    pixman
    xcbutilwm
    libX11
    libcap
    xcbutilimage
    xcbutilerrors
    mesa_noglu
    libpng
    ffmpeg
    libglvnd
  ];

  meta = {
    inherit (inputs.wlroots) description homepage;
    license = stdenv.lib.licenses.mit;
    platforms = stdenv.lib.platforms.linux;
    maintainers = [
      {
        email = "john@insane.se";
        github = "johnae";
        name = "John Axel Eriksson";
      }
    ];
  };
}
