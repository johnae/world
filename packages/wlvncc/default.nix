{
  lib,
  stdenv,
  meson,
  pkg-config,
  ninja,
  scdoc,
  wayland,
  wayland-protocols,
  pixman,
  libxkbcommon,
  libdrm,
  cmake,
  ffmpeg,
  aml,
  libGL,
  zlib,
  lzo,
  libpng,
  libjpeg,
  mesa,
  openssl,
  inputs,
}:
stdenv.mkDerivation rec {
  pname = "wlvncc";
  version = inputs.wlvncc.rev;

  src = inputs.wlvncc;

  nativeBuildInputs = [meson pkg-config ninja scdoc cmake];
  buildInputs = [
    pixman
    libxkbcommon
    wayland
    wayland-protocols
    libdrm
    aml
    ffmpeg
    mesa
    libGL
    openssl
    zlib
    lzo
    libpng
    libjpeg
  ];

  NIX_CFLAGS_COMPILE = "-Wno-unused-variable";

  meta = with lib; {
    description = "A VNC server for wlroots based Wayland compositors";
    longDescription = ''
      This is a VNC server for wlroots based Wayland compositors. It attaches
      to a running Wayland session, creates virtual input devices and exposes a
      single display via the RFB protocol. The Wayland session may be a
      headless one, so it is also possible to run wayvnc without a physical
      display attached.
    '';
    inherit (src.meta) homepage;
    changelog = "https://github.com/any1/wayvnc/releases/tag/v${version}";
    license = licenses.isc;
    platforms = platforms.linux;
    maintainers = with maintainers; [primeos];
  };
}
