{
  stdenv,
  lib,
  meson,
  ninja,
  pkg-config,
  wayland,
  wayland-protocols,
  cairo,
  libxkbcommon,
  libjpeg,
  git,
  systemd,
  scdoc,
  inputs,
}: let
  version = inputs.slurp.rev;
in
  stdenv.mkDerivation {
    name = "slurp-${version}";
    inherit version;

    src = inputs.slurp;

    nativeBuildInputs = [meson ninja pkg-config git scdoc];
    buildInputs = [
      wayland
      wayland-protocols
      cairo
      libjpeg
      libxkbcommon
      systemd
    ];

    meta = {
      description = "Slurp";
      license = lib.licenses.mit;
      platforms = lib.platforms.linux;
    };
  }
