{ stdenv
, meson
, ninja
, pkgconfig
, git
, asciidoc
, libxslt
, docbook_xsl
, scdoc
, wayland
, wayland-protocols
, libxkbcommon
, cairo
, pam
, gdk_pixbuf
, inputs
, buildDocs ? true
}:
let
  version = inputs.swaylock.rev;
in
stdenv.mkDerivation {
  name = "swaylock-${version}";
  inherit version;

  src = inputs.swaylock;

  nativeBuildInputs = [ meson ninja pkgconfig git ]
    ++ stdenv.lib.optional buildDocs [ scdoc asciidoc libxslt docbook_xsl ];
  buildInputs = [ wayland wayland-protocols cairo pam gdk_pixbuf libxkbcommon ];

  mesonFlags = [ "-Dauto_features=enabled" ];

  enableParallelBuilding = true;

  meta = {
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
