{
  lib,
  git,
  ncurses,
  openssl,
  pkg-config,
  rustPlatform,
  sqlite,
  inputs,
}:
rustPlatform.buildRustPackage rec {
  pname = "git-branchless";
  version = inputs.git-branchless.rev;

  src = inputs.git-branchless;

  cargoSha256 = "sha256-iax7hDn03dOk8xrKqFF2vTFlYdwb358QJ4GYEGeSv7c=";

  nativeBuildInputs = [pkg-config];

  buildInputs = [
    ncurses
    openssl
    sqlite
  ];

  doCheck = false;

  preCheck = ''
    # Tests require path to git.
    export PATH_TO_GIT=${git}/bin/git
  '';

  meta = with lib; {
    description = "A suite of tools to help you visualize, navigate, manipulate, and repair your commit history";
    homepage = "https://github.com/arxanas/git-branchless";
    license = licenses.asl20;
    maintainers = with maintainers; [nh2];
  };
}
