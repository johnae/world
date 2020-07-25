{ stdenv
, rustPlatform
, pkgconfig
, dbus
, libpulseaudio
, alsaLib
, openssl
, inputs
}:

rustPlatform.buildRustPackage rec {
  pname = "spotifyd";
  version = inputs.spotifyd.rev;

  src = inputs.spotifyd;
  cargoSha256 = "sha256-G1/sjLXpBV9JpSHvlBy/s4KXsYB0iyhToZJpVl7pk0Y=";

  nativeBuildInputs = [ pkgconfig ];

  buildInputs = [ libpulseaudio openssl pkgconfig alsaLib dbus ];

  doCheck = false;
  #cargoBuildFlags = [ "--features pulseaudio_backend,dbus_mpris" ];
  cargoBuildFlags = [ "--features pulseaudio_backend" ];

  meta = with stdenv.lib; {
    license = licenses.gpl3;
    maintainers = [
      {
        email = "john@insane.se";
        github = "johnae";
        name = "John Axel Eriksson";
      }
    ];
  };
}
