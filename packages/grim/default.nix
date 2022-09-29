{
  stdenv,
  lib,
  meson,
  ninja,
  pkg-config,
  wayland,
  wayland-protocols,
  cairo,
  libjpeg,
  git,
  systemd,
  scdoc,
  inputs,
}:
stdenv.mkDerivation {
  name = "grim-${inputs.grim.rev}";
  version = inputs.grim.rev;

  src = inputs.grim;

  nativeBuildInputs = [meson ninja pkg-config git scdoc];
  buildInputs = [wayland wayland-protocols cairo libjpeg systemd];

  meta = {
    description = "image grabber for wayland compositors";
    homepage = "https://wayland.emersion.fr/grim/";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
  };
}
