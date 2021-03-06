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
  extraEmacsPackages = epkgs: [ epkgs.org-plus-contrib ];
}
