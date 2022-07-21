{
  lib,
  rustPlatform,
  pkgconfig,
  dbus,
  libpulseaudio,
  alsaLib,
  openssl,
  inputs,
}:
rustPlatform.buildRustPackage {
  pname = "spotifyd";
  version = inputs.spotifyd.rev;

  src = inputs.spotifyd;
  cargoSha256 = "sha256-+duAjTi3eaROuRyVyJoUDq7efUbcOOOqj8CovCzucXc=";

  nativeBuildInputs = [pkgconfig];

  buildInputs = [libpulseaudio openssl pkgconfig alsaLib dbus];

  doCheck = false;
  cargoBuildFlags = ["--features pulseaudio_backend,dbus_mpris"];

  meta = {
    license = lib.licenses.gpl3;
    platforms = lib.platforms.linux; ## in this case it's true
    maintainers = [
      {
        email = "john@insane.se";
        github = "johnae";
        name = "John Axel Eriksson";
      }
    ];
  };
}
