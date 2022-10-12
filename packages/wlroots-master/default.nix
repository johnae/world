{
  stdenv,
  lib,
  meson,
  ninja,
  pkg-config,
  wayland,
  libGL,
  wayland-protocols,
  libinput,
  libxkbcommon,
  pixman,
  xcbutilwm,
  libX11,
  libcap,
  xcbutilimage,
  xcbutilrenderutil,
  xcbutilerrors,
  mesa,
  libglvnd,
  libpng,
  libuuid,
  seatd,
  libdrm,
  ffmpeg,
  xwayland,
  vulkan-headers,
  vulkan-loader,
  glslang,
  enableXWayland ? true,
  inputs,
}:
stdenv.mkDerivation {
  name = "wlroots-${inputs.wlroots.rev}";
  version = inputs.wlroots.rev;

  src = inputs.wlroots;

  outputs = ["out"];

  nativeBuildInputs = [meson ninja pkg-config xwayland];

  mesonFlags =
    lib.optional (!enableXWayland) "-Dxwayland=disabled";

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
    seatd
    libdrm
    xcbutilimage
    xcbutilerrors
    xcbutilrenderutil
    mesa
    libpng
    ffmpeg
    libglvnd
    libuuid
    vulkan-headers
    vulkan-loader
    glslang
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
