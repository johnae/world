{
  lib,
  stdenv,
  aml,
  cmake,
  fetchFromGitHub,
  jansson,
  libX11,
  libdrm,
  libxkbcommon,
  mesa,
  meson,
  neatvnc,
  ninja,
  pam,
  pixman,
  pkg-config,
  scdoc,
  wayland,
  wayland-scanner,
  inputs,
}:
stdenv.mkDerivation rec {
  pname = "wayvnc";
  version = inputs.wayvnc.rev;

  src = inputs.wayvnc;

  nativeBuildInputs = [meson pkg-config ninja scdoc wayland-scanner cmake];
  buildInputs = [pixman libxkbcommon wayland neatvnc libdrm libX11 aml pam mesa jansson];

  meta = with lib; {
    description = "A VNC server for wlroots based Wayland compositors";
    longDescription = ''
      This is a VNC server for wlroots based Wayland compositors. It attaches
      to a running Wayland session, creates virtual input devices and exposes a
      single display via the RFB protocol. The Wayland session may be a
      headless one, so it is also possible to run wayvnc without a physical
      display attached.
    '';
    inherit (src.meta) homepage;
    changelog = "https://github.com/any1/wayvnc/releases/tag/v${version}";
    license = licenses.isc;
    platforms = platforms.linux;
    maintainers = with maintainers; [primeos];
  };
}
