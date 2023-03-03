{
  stdenv,
  lib,
  cmake,
  ffmpeg,
  glslang,
  hwdata,
  libGL,
  libX11,
  libcap,
  libdisplay-info,
  libdrm,
  libglvnd,
  libinput,
  libpng,
  libuuid,
  libxkbcommon,
  mesa,
  meson,
  ninja,
  pixman,
  pkg-config,
  seatd,
  vulkan-headers,
  vulkan-loader,
  wayland,
  wayland-protocols,
  xcbutilerrors,
  xcbutilimage,
  xcbutilrenderutil,
  xcbutilwm,
  xwayland,
  enableXWayland ? true,
  inputs,
}:
stdenv.mkDerivation {
  name = "wlroots-${inputs.wlroots.rev}";
  version = inputs.wlroots.rev;

  src = inputs.wlroots;

  outputs = ["out"];

  nativeBuildInputs = [meson cmake ninja pkg-config xwayland];

  mesonFlags =
    lib.optional (!enableXWayland) "-Dxwayland=disabled";

  buildInputs = [
    ffmpeg
    glslang
    hwdata
    libGL
    libX11
    libcap
    libdisplay-info
    libdrm
    libglvnd
    libinput
    libpng
    libuuid
    libxkbcommon
    mesa
    pixman
    seatd
    vulkan-headers
    vulkan-loader
    wayland
    wayland-protocols
    xcbutilerrors
    xcbutilimage
    xcbutilrenderutil
    xcbutilwm
  ];

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
