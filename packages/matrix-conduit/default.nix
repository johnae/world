{
  stdenv,
  lib,
  fetchFromGitLab,
  rustPlatform,
  pkgs,
  inputs,
}:
rustPlatform.buildRustPackage rec {
  pname = "matrix-conduit";
  version = inputs.matrix-conduit.rev;

  src = inputs.matrix-conduit;

  cargoSha256 = "sha256-Sxv2ecfLdo3eJG6w545oVA3Cm3QkWUx0NPCi8j6bQ4w=";

  doCheck = false;

  nativeBuildInputs = with pkgs; [
    rustPlatform.bindgenHook
  ];

  buildInputs = with pkgs; [
    pkg-config
    rocksdb
  ];

  cargoBuildFlags = "--bin conduit";

  meta = with lib; {
    broken = stdenv.isDarwin;
    description = "A Matrix homeserver written in Rust";
    homepage = "https://conduit.rs/";
    license = licenses.asl20;
    maintainers = with maintainers; [pstn piegames pimeys];
  };
}
