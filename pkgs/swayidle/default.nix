{ stdenv
, lib
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
stdenv.mkDerivation {
  name = "swayidle-${version}";
  inherit version;

  src = inputs.swayidle;

  nativeBuildInputs = [ meson ninja pkgconfig git ]
    ++ lib.optional buildDocs [ scdoc asciidoc libxslt docbook_xsl ];
  buildInputs = [ wayland wayland-protocols systemd ];

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
