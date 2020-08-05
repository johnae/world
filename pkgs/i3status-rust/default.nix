{ stdenv, rustPlatform, pkgconfig, dbus, libpulseaudio, inputs }:
rustPlatform.buildRustPackage {
  pname = inputs.i3status-rust.repo;
  version = inputs.i3status-rust.rev;

  src = inputs.i3status-rust;
  cargoSha256 = "0zcnyy2x09vsknmk99nj71ngxvdrz9mvwlzyd9izhlpwjgc9pj6v";

  nativeBuildInputs = [ pkgconfig ];

  buildInputs = [ dbus libpulseaudio ];

  # Currently no tests are implemented, so we avoid building the package twice
  doCheck = false;

  meta = {
    description =
      "Very resource-friendly and feature-rich replacement for i3status";
    homepage = "https://github.com/greshake/i3status-rust";
    license = stdenv.lib.licenses.gpl3;
    maintainers = [
      {
        email = "john@insane.se";
        github = "johnae";
        name = "John Axel Eriksson";
      }
    ];
    platforms = stdenv.lib.platforms.linux;
  };
}
