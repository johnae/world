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

stdenv.mkDerivation rec {
  name = "wf-recorder";
  version = inputs.wf-recorder.rev;

  src = inputs.wf-recorder;

  nativeBuildInputs = [ meson ninja pkgconfig scdoc ];

  buildInputs = [ wayland wayland-protocols ffmpeg libpulseaudio ];

  mesonFlags = [ "-Dopencl=disabled" ];

  meta = with stdenv.lib; {
    license = licenses.mit;
    platforms = platforms.linux;
  };
}
