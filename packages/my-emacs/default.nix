{
  isync,
  mu,
  pandoc,
  pass,
  fish,
  kubectl,
  kubectx,
  texlive,
  google-cloud-sdk,
  wl-clipboard,
  emacsWithPackagesFromUsePackage,
  pkgs,
  inputs,
  lib,
  makeWrapper,
  emacsPkg ?
    pkgs.emacsUnstable.override {
      nativeComp = true;
      withNS = false;
      withX = false;
      withGTK2 = false;
      withGTK3 = false;
      withWebP = false;
    },
  ...
}:
emacsWithPackagesFromUsePackage {
  alwaysEnsure = true;
  alwaysTangle = true;
  config = ./emacs.org;

  extraEmacsPackages = epkgs: [
    epkgs.markdown-mode
    epkgs.tree-sitter
    (epkgs.tree-sitter-langs.withPlugins (
      p:
        epkgs.tree-sitter-langs.plugins
        ++ [
          p.tree-sitter-markdown
        ]
    ))
  ];
  package =
    emacsPkg.overrideAttrs
    (oa: {
      nativeBuildInputs = oa.nativeBuildInputs ++ [makeWrapper];
      postInstall = ''
        ${oa.postInstall}
        wrapProgram $out/bin/emacs \
          --set MU4E_LOAD_PATH "${mu}/share/emacs/site-lisp/mu4e" \
          --prefix PATH : ${pkgs.lib.makeBinPath [fish mu isync pandoc pass wl-clipboard kubectl kubectx google-cloud-sdk texlive.combined.scheme-full]}
      '';
      meta.platforms = lib.platforms.linux;
    });
}
