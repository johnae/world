{
  pkgs,
  lib,
  ...
}: let
  emacsInit = pkgs.replaceVars ./config.el {
    extras = lib.concatStringsSep "\n" (map (n: builtins.readFile ./extras/${n}) (
      lib.mapAttrsToList (name: _: name) (lib.filterAttrs (_: type: type == "regular") (builtins.readDir ./extras))
    ));
  };
in {
  # services.emacs = {
  #   enable = true;
  #   package = pkgs.my-emacs;
  #   socketActivation.enable = true;
  # };
  home.file.".emacs.d/early-init.el".source = pkgs.writeText "early-init.el" ''
    ;; Startup speed, annoyance suppression
    (setq gc-cons-threshold 10000000)
    (setq byte-compile-warnings '(not obsolete))
    (setq warning-suppress-log-types '((comp) (bytecomp)))
    (setq native-comp-async-report-warnings-errors 'silent)

    ;; Silence startup message
    (setq inhibit-startup-echo-area-message (user-login-name))

    ;; Default frame configuration: full screen, good-looking title bar on macOS
    (setq frame-resize-pixelwise t)
    (tool-bar-mode -1)                      ; All these tools are in the menu-bar anyway
    (setq default-frame-alist '((fullscreen . maximized)

                                ;; Setting the face in here prevents flashes of
                                ;; color as the theme gets activated
                                (background-color . "#000000")
                                (ns-appearance . dark)
                                (ns-transparent-titlebar . t)))

  '';
  home.file.".emacs.d/init.el".source = emacsInit;
  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      config = emacsInit;
      defaultInitFile = false;
      package =
        if pkgs.stdenv.isDarwin
        then pkgs.emacs30
        else pkgs.emacs30-pgtk;
      extraEmacsPackages = epkgs:
        with epkgs; [
          tree-sitter-langs
          tree-sitter
          treesit-grammars.with-all-grammars
        ];
    };
  };
  systemd.user.services.emacs.Service = {
    Environment = [
      ''COLORTERM="truecolor"''
    ];
    LimitNOFILE = 16384;
  };
}
