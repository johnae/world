{
  pkgs,
  emacsWithPackagesFromUsePackage,
  emacsPkg ? pkgs.emacs-unstable-nox,
  inputs,
  ...
}:
emacsWithPackagesFromUsePackage {
  alwaysEnsure = true;
  alwaysTangle = true;
  config = ./emacs.org;
  defaultInitFile = pkgs.callPackage ./config.nix {loadPaths = [inputs.emacs-copilot];};
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
  package = emacsPkg;
}
