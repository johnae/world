{ stdenv
, lib
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
, xcbutilrenderutil
, xcbutilerrors
, mesa_noglu
, libglvnd
, libpng
, libuuid
, ffmpeg
, xwayland
, inputs
}:

stdenv.mkDerivation {
  name = "wlroots-${inputs.wlroots.rev}";
  version = inputs.wlroots.rev;

  src = inputs.wlroots;

  outputs = [ "out" ];

  nativeBuildInputs = [ meson ninja pkgconfig xwayland ];

  mesonFlags = [
    "-Dlibcap=enabled"
    "-Dlogind-provider=systemd"
    "-Dxwayland=enabled"
    "-Dx11-backend=enabled"
    "-Dxcb-icccm=enabled"
    "-Dxcb-errors=enabled"
    "-Dxcb-xkb=enabled"
    "-Dlibseat=disabled"
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
    xcbutilrenderutil
    mesa_noglu
    libpng
    ffmpeg
    libglvnd
    libuuid
  ];

  meta = {
    inherit (inputs.wlroots) description homepage;
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
