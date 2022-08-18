{
  lib,
  d2n,
  pkgs,
  inputs,
}:
(d2n.makeOutputs {
  inherit pkgs;
  source = inputs.spotifyd;
  packageOverrides."^.*".addDeps = {
    nativeBuildInputs = old: old ++ [pkgs.pkgconfig];
    buildInputs = old: old ++ [pkgs.libpulseaudio pkgs.openssl pkgs.alsaLib pkgs.dbus];
    doCheck = false;
    cargoBuildFlags = ["--features pulseaudio_backend,dbus_mpris"];
  };
})
.packages
.spotifyd
