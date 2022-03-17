{
  stdenv,
  lib,
  meson,
  ninja,
  pkgconfig,
  substituteAll,
  scdoc,
  wayland,
  wayland-protocols,
  libxkbcommon,
  swaybg,
  pcre2,
  json_c,
  dbus,
  pango,
  cairo,
  libinput,
  libcap,
  libdrm,
  pam,
  gdk-pixbuf,
  libevdev,
  librsvg,
  wlroots,
  inputs,
  isNixOS ? true,
  enableXWayland ? true,
}:
stdenv.mkDerivation rec {
  pname = "sway-unwrapped";
  version = inputs.sway.rev;

  src = inputs.sway;

  patches = [
    ./sway-config-no-nix-store-references.patch
    ./load-configuration-from-etc.patch
    (substituteAll {
      src = ./fix-paths.patch;
      inherit swaybg;
    })
  ];

  nativeBuildInputs = [pkgconfig meson ninja scdoc];

  buildInputs = [
    wayland
    wayland-protocols
    libxkbcommon
    pcre2
    json_c
    dbus
    pango
    cairo
    libinput
    libcap
    pam
    gdk-pixbuf
    libevdev
    scdoc
    librsvg
    libdrm
    wlroots
  ];

  postPatch = ''
    date="$(date -d '@${builtins.toString inputs.sway.lastModified}' +'%b %d %Y')"
    sed -i "s/\([ \t]\)version: '\(.*\)',/\1version: '\2-${inputs.sway.shortRev} ($date)',/" meson.build
  '';

  mesonFlags = [
    "-Ddefault-wallpaper=false"
    "-Dxwayland=enabled"
    "-Dgdk-pixbuf=enabled"
    "-Dtray=enabled"
    "-Dman-pages=enabled"
    "-Dsd-bus-provider=libsystemd"
  ];

  enableParallelBuilding = true;

  meta = {
    description = "i3-compatible tiling Wayland compositor";
    homepage = https://swaywm.org;
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
