{ stdenv
, lib
, rustPackages_1_45
, pkgconfig
, dbus
, libpulseaudio
, alsaLib
, openssl
, inputs
}:

rustPackages_1_45.rustPlatform.buildRustPackage {
  pname = "spotifyd";
  version = inputs.spotifyd.rev;

  src = inputs.spotifyd;
  cargoSha256 = "sha256-fQ1pTglLcBLCRukx863ErxG3dHCsxMytOJ1LZewA/bg=";

  nativeBuildInputs = [ pkgconfig ];

  buildInputs = [ libpulseaudio openssl pkgconfig alsaLib dbus ];

  doCheck = false;
  #cargoBuildFlags = [ "--features pulseaudio_backend,dbus_mpris" ];
  cargoBuildFlags = [ "--features pulseaudio_backend" ];

  meta = {
    license = lib.licenses.gpl3;
    maintainers = [
      {
        email = "john@insane.se";
        github = "johnae";
        name = "John Axel Eriksson";
      }
    ];
  };
}
