{
  stdenv,
  lib,
  asciidoc,
  cairo,
  docbook_xsl,
  gdk-pixbuf,
  git,
  libdrm,
  libxkbcommon,
  libxslt,
  meson,
  ninja,
  pam,
  pkg-config,
  scdoc,
  wayland,
  wayland-protocols,
  inputs,
  buildDocs ? true,
}: let
  version = inputs.swaylock.rev;
in
  stdenv.mkDerivation {
    name = "swaylock-${version}";
    inherit version;

    src = inputs.swaylock;

    nativeBuildInputs =
      [meson ninja pkg-config git]
      ++ lib.optional buildDocs [scdoc asciidoc libxslt docbook_xsl];
    buildInputs = [wayland wayland-protocols cairo pam gdk-pixbuf libdrm libxkbcommon];

    mesonFlags = ["-Dauto_features=enabled"];

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
