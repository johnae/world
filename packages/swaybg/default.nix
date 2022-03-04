{ stdenv
, lib
, meson
, ninja
, pkgconfig
, git
, scdoc
, wayland
, wayland-protocols
, cairo
, gdk-pixbuf
, inputs
, buildDocs ? true
}:

stdenv.mkDerivation {
  name = "swaybg-${inputs.swaybg.rev}";
  version = inputs.swaybg.rev;

  src = inputs.swaybg;

  nativeBuildInputs = [ meson ninja pkgconfig git ]
    ++ lib.optional buildDocs [ scdoc ];
  buildInputs = [ wayland wayland-protocols cairo gdk-pixbuf ];

  mesonFlags = [ "-Dauto_features=enabled" ];

  enableParallelBuilding = true;

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
