{ stdenv, pkgs, ... }:

with pkgs;

stdenv.mkDerivation rec {
  version = "1.0.0";
  name = "btr-snap-${version}";

  buildInputs =
    [ bashInteractive btrfs-progs openssh makeWrapper coreutils pv hostname ];

  src = ./.;

  installPhase = ''
    runHook preInstall
    for scpt in $(ls ${./bin}); do
      install -D ${./bin}/$scpt $out/script/$scpt
    done
    for scpt in $out/script/*; do
        local sname=$(basename "$scpt")
        makeWrapper "$scpt" "$out/bin/$sname" --prefix PATH ":" \
        "${openssh}/bin:${btrfs-progs}/bin:${coreutils}/bin:${pv}/bin:${hostname}/bin"
    done
    runHook postInstall
  '';

  meta = {
    description = "Btrfs snapshot and backup scripts";
    homepage = "https://github.com/johnae/btr-snap";
    license = "MIT";
  };

}
