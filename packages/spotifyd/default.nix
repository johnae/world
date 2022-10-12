{
  lib,
  d2n,
  pkgs,
  pkg-config,
  libpulseaudio,
  openssl,
  alsa-lib,
  dbus,
  inputs,
}:
(d2n.makeOutputs {
  inherit pkgs;
  source = inputs.spotifyd;
  packageOverrides."^.*".addDeps = {
    nativeBuildInputs = old: old ++ [pkg-config];
    buildInputs = old: old ++ [libpulseaudio openssl alsa-lib dbus];
    doCheck = false;
    cargoBuildFlags = ["--features pulseaudio_backend,dbus_mpris"];
  };
})
.packages
.spotifyd
