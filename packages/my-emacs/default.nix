{ emacsPackages
, fetchgit
, writeText
, notmuch
, pandoc
, pass
, fish
, kubectl
, kubectx
, google-cloud-sdk
, wl-clipboard
, emacsWithPackagesFromUsePackage
, pkgs
, makeWrapper
, ...
}:
let
  jl-encrypt = emacsPackages.melpaBuild {
    pname = "jl-encrypt";
    version = "20200904";

    src = fetchgit {
      url = "https://gitlab.com/lechten/defaultencrypt.git";
      rev = "83fb2d46061127b51d65d3548ffbe18642398a52";
      sha256 = "sha256-jtTXuHy2EsJroyt5PX+L3rJaX3xrdjcLnNpk3w9/470=";
    };

    recipe = writeText "jl-encrypt-recipe" ''
      (jl-encrypt :fetcher git
                  :url "https://gitlab.com/lechten/defaultencrypt.git"
                  :files (:defaults))
    '';
  };
in
emacsWithPackagesFromUsePackage {
  alwaysEnsure = true;
  alwaysTangle = true;
  config = ./emacs.org;
  package = pkgs.emacsPgtkGcc.overrideAttrs
    (oa: {
      nativeBuildInputs = oa.nativeBuildInputs ++ [ makeWrapper ];
      postInstall = ''
        ${oa.postInstall}
        wrapProgram $out/bin/emacs \
          --set NOTMUCH_LOAD_PATH "${notmuch.emacs}/share/emacs/site-lisp" \
          --prefix PATH : ${pkgs.lib.makeBinPath [ fish notmuch pandoc pass wl-clipboard kubectl kubectx google-cloud-sdk ]}
      '';
    });
  extraEmacsPackages = epkgs: [ jl-encrypt epkgs.org-plus-contrib ];
}
