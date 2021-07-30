{ stdenv
, lib
, meson
, ninja
, pkgconfig
, scdoc
, makeWrapper
, git
, cairo
, pango
, wayland
, wayland-protocols
, pipewire
, libdrm
, systemd
, gdk_pixbuf
, inputs
}:

stdenv.mkDerivation {
  name = "xdg-desktop-portal-wlr";
  version = inputs.xdg-desktop-portal-wlr.rev;

  src = inputs.xdg-desktop-portal-wlr;

  nativeBuildInputs = [ meson ninja pkgconfig git scdoc makeWrapper ];

  buildInputs = [
    cairo
    pango
    wayland
    wayland-protocols
    systemd
    gdk_pixbuf
    pipewire
    libdrm
  ];

  mesonFlags = [ "-Dauto_features=auto" ];

  meta = {
    inherit (inputs.xdg-desktop-portal-wlr) description homepage;
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
  };
}
