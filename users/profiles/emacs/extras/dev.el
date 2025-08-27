;;; dev-ts.el --- Emacs config (Tree-sitter first)  -*- lexical-binding: t; -*-

;;; Commentary:
;; Prefer built-in Tree-sitter modes where available. Fall back to classic modes
;; only when needed (e.g., markdown-mode; yaml-mode as a fallback). Keep startup
;; fast by autoloading via :mode/:hook/:commands and avoid eager requires.

;;; Code:

;; --- Tree-sitter preferences -------------------------------------------------

;; Crisper highlighting; higher values can cost a little CPU on large files.
(setq treesit-font-lock-level 4)

(defun my/ts-remap (from to)
  "Safely remap FROM major mode to tree-sitter TO if TO exists."
  (when (fboundp to)
    (add-to-list 'major-mode-remap-alist (cons from to))))

;; Prefer TS modes; only remap if target mode is defined on this Emacs.
(mapc (lambda (x) (apply #'my/ts-remap x))
      '((c-mode           c-ts-mode)
        (c++-mode         c++-ts-mode)
        (cmake-mode       cmake-ts-mode)
        (bash-mode        bash-ts-mode)
        (sh-mode          bash-ts-mode)      ; drop if you edit strict POSIX sh
        (javascript-mode  js-ts-mode)
        (js2-mode         js-ts-mode)
        (typescript-mode  typescript-ts-mode)
        (css-mode         css-ts-mode)
        (json-mode        json-ts-mode)
        (conf-toml-mode   toml-ts-mode)
        (go-mode          go-ts-mode)
        (nix-mode         nix-ts-mode)
        (python-mode      python-ts-mode)
        (ruby-mode        ruby-ts-mode)
        (java-mode        java-ts-mode)
        (rust-mode        rust-ts-mode)
        (yaml-mode        yaml-ts-mode)))

;; Auto pairing in programming buffers.
(add-hook 'prog-mode-hook #'electric-pair-mode)

;; --- Markdown (no first-class built-in TS yet) -------------------------------
;; Keep markdown-mode for full features.
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :hook (markdown-mode . visual-line-mode))

;; --- YAML: try TS first; keep yaml-mode only as fallback ---------------------
(use-package yaml-mode
  :ensure t
  :unless (fboundp 'yaml-ts-mode)
  :mode "\\.ya?ml\\'")

;; --- Nix TS (external package) -----------------------------------------------
(use-package nix-ts-mode
  :ensure t
  :mode "\\.nix\\'")

;; --- Justfile ----------------------------------------------------------------
(use-package just-mode
  :ensure t
  :mode (("\\`[Jj]ustfile\\'" . just-mode)
         ("\\.just\\'"        . just-mode)))

;; --- Eglot (built-in LSP client) ---------------------------------------------
(use-package eglot
  :defer t
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)
  :config
  ;; Reduce event-buffer overhead (Emacs 29+). Older: silence jsonrpc logs.
  (if (boundp 'eglot-events-buffer-size)
      (setq eglot-events-buffer-size 0)
    (fset #'jsonrpc--log-event #'ignore))
  ;; Example: use jdtls for Java without lsp-java.
  (add-to-list 'eglot-server-programs '(java-mode . ("jdtls")))
  ;; Increase I/O throughput for LSP servers.
  (setq read-process-output-max (* 3 1024 1024))
  :hook ((rust-mode rust-ts-mode
          java-mode java-ts-mode
          nix-mode  nix-ts-mode
          json-mode json-ts-mode
          yaml-mode yaml-ts-mode
          python-mode python-ts-mode
          typescript-mode typescript-ts-mode
          js-mode js-ts-mode
          go-mode go-ts-mode)
         . eglot-ensure))

;; --- Apheleia (format on save, but enabled after startup) --------------------
(use-package apheleia
  :ensure t
  :commands (apheleia-global-mode)
  :init
  (add-hook 'after-init-hook #'apheleia-global-mode)
  :config
  ;; Nix formatter
  (push '(alejandra . ("alejandra" "-")) apheleia-formatters)
  (setf (alist-get 'nix-mode apheleia-mode-alist) 'alejandra)
  (setf (alist-get 'nix-ts-mode apheleia-mode-alist) 'alejandra))
