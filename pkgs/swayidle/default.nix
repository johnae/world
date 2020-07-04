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
, systemd
, inputs
, buildDocs ? true
}:
let
  version = inputs.swayidle.rev;
in
stdenv.mkDerivation rec {
  name = "swayidle-${version}";
  inherit version;

  src = inputs.swayidle;

  nativeBuildInputs = [ meson ninja pkgconfig git ]
    ++ stdenv.lib.optional buildDocs [ scdoc asciidoc libxslt docbook_xsl ];
  buildInputs = [ wayland wayland-protocols systemd ];

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
