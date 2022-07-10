{
  lib,
  stdenv,
  fetchFromGitHub,
  meson,
  pkg-config,
  ninja,
  pixman,
  gnutls,
  libdrm,
  libjpeg_turbo,
  zlib,
  aml,
  mesa,
  cmake,
  ffmpeg,
  inputs,
}:
stdenv.mkDerivation rec {
  pname = "neatvnc";
  version = inputs.neatvnc.rev;

  src = inputs.neatvnc;

  nativeBuildInputs = [meson pkg-config ninja cmake];
  buildInputs = [pixman gnutls libdrm libjpeg_turbo zlib aml ffmpeg mesa];

  meta = with lib; {
    description = "A VNC server library";
    longDescription = ''
      This is a liberally licensed VNC server library that's intended to be
      fast and neat. Goals:
      - Speed
      - Clean interface
      - Interoperability with the Freedesktop.org ecosystem
    '';
    inherit (src.meta) homepage;
    changelog = "https://github.com/any1/neatvnc/releases/tag/v${version}";
    license = licenses.isc;
    platforms = platforms.linux;
    maintainers = with maintainers; [primeos];
  };
}
