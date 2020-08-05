{ stdenv
, meson
, ninja
, pkgconfig
, wayland
, scdoc
, ffmpeg
, wayland-protocols
, libpulseaudio
, inputs
}:

stdenv.mkDerivation {
  name = "wf-recorder";
  version = inputs.wf-recorder.rev;

  src = inputs.wf-recorder;

  nativeBuildInputs = [ meson ninja pkgconfig scdoc ];

  buildInputs = [ wayland wayland-protocols ffmpeg libpulseaudio ];

  mesonFlags = [ "-Dopencl=disabled" ];

  meta = {
    license = stdenv.lib.licenses.mit;
    platforms = stdenv.lib.platforms.linux;
  };
}
