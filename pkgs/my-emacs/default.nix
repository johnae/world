{ emacsPackages
, fetchgit
, writeText
, mu
, emacsWithPackagesFromUsePackage
, pkgs
, makeWrapper
, ...
}:
let
  jl-encrypt = emacsPackages.melpaBuild {
    pname = "jl-encrypt";
    version = "20190618";

    src = fetchgit {
      url = "https://gitlab.com/lechten/defaultencrypt.git";
      rev = "ba07acc8e9fd692534c39c7cdad0a19dc0d897d9";
      sha256 = "1ln7h1syx7yi7bqvirv90mk4rvwxg4zm1wvfcvhfh64s3hqrbfgl";
    };

    recipe = writeText "jl-encrypt-recipe" ''
      (jl-encrypt :fetcher git
                  :url "https://gitlab.com/lechten/defaultencrypt.git"
                  :files (:defaults))
    '';
  };

  config = pkgs.callPackage ./config.nix { };
in
emacsWithPackagesFromUsePackage {
  config = builtins.readFile config.emacsConfig;
  package = pkgs.emacsGit-nox.overrideAttrs
    (oa: {
      nativeBuildInputs = oa.nativeBuildInputs ++ [ makeWrapper ];
      postInstall = ''
        ${oa.postInstall}
        wrapProgram $out/bin/emacs \
          --set TERM xterm-24bits
        wrapProgram $out/bin/emacsclient \
          --set TERM xterm-24bits
      '';
    });
  extraEmacsPackages = epkgs: [ jl-encrypt ];
}
