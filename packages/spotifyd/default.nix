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
  cargoSha256 = "sha256-KIWnHdeJFhrLjuMBEqrRWO6jJEU3+9JPXvNxMdJXVHM=";

  nativeBuildInputs = [pkgconfig];

  buildInputs = [libpulseaudio openssl pkgconfig alsaLib dbus];

  doCheck = false;
  #cargoBuildFlags = [ "--features pulseaudio_backend,dbus_mpris" ];
  cargoBuildFlags = ["--features pulseaudio_backend"];

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
