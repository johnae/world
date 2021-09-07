{ lib
, stdenv
, meson
, ninja
, pkg-config
, bison
, check
, flex
, librsvg
, libstartup_notification
, libxkbcommon
, pango
, wayland
, wayland-protocols
, xcbutilwm
, xcbutilxrm
, xcbutilcursor
, plugins ? [] ## just here for compatibility
, inputs
}:

stdenv.mkDerivation rec {
  pname = "rofi-wayland";
  version = inputs.rofi-wayland.rev;

  ## HACK around flakes not supporting submodules
  src = builtins.fetchGit {
    url = "https://github.com/lbonn/rofi";
    ref = "wayland";
    inherit (inputs.rofi-wayland) rev;
    submodules = true;
  };

  nativeBuildInputs = [
    ninja
    meson
    pkg-config
  ];

  buildInputs = [
    bison
    check
    flex
    librsvg
    libstartup_notification
    libxkbcommon
    pango
    wayland
    wayland-protocols
    xcbutilcursor
    xcbutilwm
    xcbutilxrm
  ];

  mesonFlags = [
    "-Dwayland=enabled"
  ];

  # Fixes:
  # ../source/rofi-icon-fetcher.c:190:17: error: format not a string literal and no format arguments [-Werror=format-security]
  hardeningDisable = [ "format" ];

  doCheck = true;

  meta = with lib; {
    description = "Window switcher, run dialog and dmenu replacement (built for Wayland)";
    homepage = "https://github.com/lbonn/rofi";
    license = licenses.mit;
    platforms = platforms.linux;
  };
}
