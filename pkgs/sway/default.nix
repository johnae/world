{ stdenv
, meson
, ninja
, pkgconfig
, scdoc
, freerdp
, wayland
, wayland-protocols
, libxkbcommon
, pcre
, json_c
, dbus
, pango
, cairo
, libinput
, libcap
, pam
, gdk_pixbuf
, libevdev
, wlroots
, inputs
, buildDocs ? true
}:
stdenv.mkDerivation rec {
  name = "sway-${inputs.sway.rev}";
  version = inputs.sway.rev;

  src = inputs.sway;

  nativeBuildInputs = [ pkgconfig meson ninja ]
    ++ stdenv.lib.optional buildDocs scdoc;

  buildInputs = [
    wayland
    wayland-protocols
    libxkbcommon
    pcre
    json_c
    dbus
    pango
    cairo
    libinput
    libcap
    pam
    gdk_pixbuf
    freerdp
    wlroots
    libevdev
    scdoc
  ];

  postPatch = ''
    sed -iE "s/version: '1.0',/version: '${version}',/" meson.build
  '';

  mesonFlags = [
    "-Ddefault-wallpaper=false"
    "-Dxwayland=enabled"
    "-Dgdk-pixbuf=enabled"
    "-Dtray=enabled"
  ] ++ stdenv.lib.optional buildDocs "-Dman-pages=enabled";

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
