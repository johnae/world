{
  pkgs,
  lib,
  inputs,
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
    ;; MASSIVE startup optimizations
    (setq gc-cons-threshold most-positive-fixnum
          gc-cons-percentage 0.6)
    
    ;; Prevent package.el loading packages prior to init.el
    (setq package-enable-at-startup nil)
    
    ;; Inhibit resizing frame
    (setq frame-inhibit-implied-resize t)
    
    ;; Disable file handler checking during startup
    (defvar default-file-name-handler-alist file-name-handler-alist)
    (setq file-name-handler-alist nil)
    
    ;; Prevent unwanted runtime compilation for native-comp
    (setq native-comp-deferred-compilation nil
          native-comp-async-report-warnings-errors 'silent)
    
    ;; Silence warnings
    (setq byte-compile-warnings '(not obsolete))
    (setq warning-suppress-log-types '((comp) (bytecomp)))
    
    ;; UI optimizations
    (push '(menu-bar-lines . 0) default-frame-alist)
    (push '(tool-bar-lines . 0) default-frame-alist)
    (push '(vertical-scroll-bars) default-frame-alist)
    (setq frame-resize-pixelwise t)
    
    ;; Silence startup message
    (setq inhibit-startup-echo-area-message (user-login-name))
    
    ;; Default frame configuration
    (setq default-frame-alist '((fullscreen . maximized)
                                (background-color . "#000000")
                                (ns-appearance . dark)
                                (ns-transparent-titlebar . t)))

    ;; Reset everything after startup
    (add-hook 'emacs-startup-hook
              (lambda ()
                (setq gc-cons-threshold (* 2 1000 1000))
                (setq gc-cons-percentage 0.1)
                (setq file-name-handler-alist default-file-name-handler-alist)))

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
          (epkgs.melpaBuild {
            pname = "claude-code";
            version = "0.1.0";
            src = inputs.claude-code-el;
          })

          (epkgs.melpaBuild {
            pname = "monet";
            version = "0.1.0";
            src = inputs.monet-el;
            propagatedBuildInputs = [websocket];
          })
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
