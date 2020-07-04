{ stdenv
, fetchFromGitHub
, meson
, ninja
, pkgconfig
, wayland
, wayland-protocols
, cairo
, libjpeg
, git
, systemd
, scdoc
, inputs
}:

stdenv.mkDerivation rec {
  name = "grim-${inputs.grim.rev}";
  version = inputs.grim.rev;

  src = inputs.grim;

  nativeBuildInputs = [ meson ninja pkgconfig git scdoc ];
  buildInputs = [ wayland wayland-protocols cairo libjpeg systemd ];

  meta = with stdenv.lib; {
    description = "image grabber for wayland compositors";
    homepage = "https://wayland.emersion.fr/grim/";
    license = licenses.mit;
    platforms = platforms.linux;
  };
}
