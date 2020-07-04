{ stdenv, rustPlatform, pkgconfig, dbus, libpulseaudio, inputs }:
rustPlatform.buildRustPackage rec {
  pname = inputs.i3status-rust.repo;
  version = inputs.i3status-rust.rev;

  src = inputs.i3status-rust;
  cargoSha256 = "0zcnyy2x09vsknmk99nj71ngxvdrz9mvwlzyd9izhlpwjgc9pj6v";

  nativeBuildInputs = [ pkgconfig ];

  buildInputs = [ dbus libpulseaudio ];

  # Currently no tests are implemented, so we avoid building the package twice
  doCheck = false;

  meta = with stdenv.lib; {
    description =
      "Very resource-friendly and feature-rich replacement for i3status";
    homepage = "https://github.com/greshake/i3status-rust";
    license = licenses.gpl3;
    maintainers = with maintainers; [ backuitist globin ];
    platforms = platforms.linux;
  };
}
