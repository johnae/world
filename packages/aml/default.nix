{
  lib,
  stdenv,
  fetchFromGitHub,
  meson,
  pkg-config,
  ninja,
  inputs,
}:
stdenv.mkDerivation rec {
  pname = "aml";
  version = inputs.aml.rev;

  src = inputs.aml;

  nativeBuildInputs = [meson pkg-config ninja];

  meta = with lib; {
    description = "Another main loop";
    inherit (src.meta) homepage;
    license = licenses.isc;
    platforms = platforms.unix;
    maintainers = with maintainers; [primeos];
    broken = stdenv.isDarwin;
  };
}
