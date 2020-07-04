{ stdenv
, meson
, ninja
, pkgconfig
, git
, scdoc
, wayland
, wayland-protocols
, cairo
, gdk_pixbuf
, inputs
, buildDocs ? true
}:

stdenv.mkDerivation rec {
  name = "swaybg-${inputs.swaybg.rev}";
  version = inputs.swaybg.rev;

  src = inputs.swaybg;

  nativeBuildInputs = [ meson ninja pkgconfig git ]
    ++ stdenv.lib.optional buildDocs [ scdoc ];
  buildInputs = [ wayland wayland-protocols cairo gdk_pixbuf ];

  mesonFlags = [ "-Dauto_features=enabled" ];

  enableParallelBuilding = true;

  meta = with stdenv.lib; {
    license = licenses.mit;
    platforms = platforms.linux;
    maintainers = with maintainers; [
      {
        email = "john@insane.se";
        github = "johnae";
        name = "John Axel Eriksson";
      }
    ];
  };
}
