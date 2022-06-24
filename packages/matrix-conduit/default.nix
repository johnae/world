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

  cargoSha256 = "sha256-1tiiMOfH35J/B08zO/DJbK0p31uHv07mVPF7Yh2q/xA=";

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
