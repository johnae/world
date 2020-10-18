{ stdenv
, rustPlatform
, pkgconfig
, dbus
, libpulseaudio
, alsaLib
, openssl
, inputs
}:

rustPlatform.buildRustPackage {
  pname = "spotifyd";
  version = inputs.spotifyd.rev;

  src = inputs.spotifyd;
  cargoSha256 = "sha256-qYdeYCVrDtp+0UVKBRYhbBF3LJMoA8u8tb8yWHC+lSs=";

  nativeBuildInputs = [ pkgconfig ];

  buildInputs = [ libpulseaudio openssl pkgconfig alsaLib dbus ];

  doCheck = false;
  #cargoBuildFlags = [ "--features pulseaudio_backend,dbus_mpris" ];
  cargoBuildFlags = [ "--features pulseaudio_backend" ];

  meta = {
    license = stdenv.lib.licenses.gpl3;
    maintainers = [
      {
        email = "john@insane.se";
        github = "johnae";
        name = "John Axel Eriksson";
      }
    ];
  };
}
