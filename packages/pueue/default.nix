{ lib
, fenix
, inputs
, installShellFiles
, makeRustPlatform
}:

let
  rustPlatform = makeRustPlatform {
    inherit (fenix.minimal) cargo rustc;
  };
in
rustPlatform.buildRustPackage rec {
  pname = "pueue";
  version = inputs.pueue.rev;

  src = inputs.pueue;

  cargoSha256 = "sha256-gvXEpVL76Q+GS1Z9oEeqi1w7nPot8GsaIP7ZFVaq4to=";

  nativeBuildInputs = [ installShellFiles ];

  doCheck = false;
  checkFlags = [ "--skip=test_single_huge_payload" "--skip=test_create_unix_socket" ];

  postInstall = ''
    for shell in bash fish zsh; do
      $out/bin/pueue completions $shell .
    done
    installShellCompletion pueue.{bash,fish} _pueue
  '';

  meta = with lib; {
    description = "A daemon for managing long running shell commands";
    homepage = "https://github.com/Nukesor/pueue";
    changelog = "https://github.com/Nukesor/pueue/raw/v${version}/CHANGELOG.md";
    license = licenses.mit;
    maintainers = [ maintainers.marsam ];
  };
}
