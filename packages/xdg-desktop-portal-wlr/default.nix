{
  stdenv,
  lib,
  cmake,
  grim,
  inih,
  iniparser,
  inputs,
  libdrm,
  makeWrapper,
  mesa,
  meson,
  ninja,
  pipewire,
  pkg-config,
  scdoc,
  slurp,
  systemd,
  wayland,
  wayland-protocols,
}:
stdenv.mkDerivation {
  name = "xdg-desktop-portal-wlr";
  version = inputs.xdg-desktop-portal-wlr.rev;

  src = inputs.xdg-desktop-portal-wlr;

  nativeBuildInputs = [
    makeWrapper
    meson
    ninja
    pkg-config
    wayland-protocols
    cmake
  ];

  buildInputs = [
    inih
    iniparser
    libdrm
    mesa
    pipewire
    scdoc
    systemd
    wayland
  ];

  mesonFlags = [
    "-Dsd-bus-provider=libsystemd"
  ];

  postInstall = ''
    wrapProgram $out/libexec/xdg-desktop-portal-wlr --prefix PATH ":" ${lib.makeBinPath [grim slurp]}
  '';

  meta = {
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
  };
}
