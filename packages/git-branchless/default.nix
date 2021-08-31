{ lib
, coreutils
, git
, ncurses
, rustPlatform
, sqlite
, inputs
}:

rustPlatform.buildRustPackage rec {
  pname = "git-branchless";
  version = inputs.git-branchless.rev;

  src = inputs.git-branchless;

  cargoSha256 = "sha256-SncSge9C/+L/MXdznuV4tMH+yunqEADUtv1BEicN1J4=";

  buildInputs = [
    ncurses
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
    maintainers = with maintainers; [ nh2 ];
  };
}
