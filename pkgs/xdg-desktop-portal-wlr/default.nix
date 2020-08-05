{ stdenv
, meson
, ninja
, pkgconfig
, scdoc
, makeWrapper
, dbus_libs
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

  meta = {
    inherit (inputs.xdg-desktop-portal-wlr) description homepage;
    license = stdenv.lib.licenses.mit;
    platforms = stdenv.lib.platforms.linux;
  };
}
