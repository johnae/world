;;; dev-ts.el --- Emacs config (Tree-sitter first)  -*- lexical-binding: t; -*-

;;; Commentary:
;; Prefer built-in Tree-sitter modes where available. Fall back to classic modes
;; only when needed (e.g., markdown-mode; yaml-mode as a fallback). Keep startup
;; fast by autoloading via :mode/:hook/:commands and avoid eager requires.

;;; Code:

;; --- Tree-sitter preferences -------------------------------------------------

;; Crisper highlighting; higher values can cost a little CPU on large files.
(setq treesit-font-lock-level 4)

;; Configure built-in tree-sitter modes with file associations
;; Each entry: (original-mode ts-mode . file-patterns)
(defvar my/ts-mode-alist
  '((c-mode           c-ts-mode           "\\.c\\'" "\\.h\\'")
    (c++-mode         c++-ts-mode         "\\.cpp\\'" "\\.cc\\'" "\\.cxx\\'" "\\.hpp\\'")
    (cmake-mode       cmake-ts-mode       "CMakeLists\\.txt\\'" "\\.cmake\\'")
    (bash-mode        bash-ts-mode        "\\.sh\\'" "\\.bash\\'")
    (sh-mode          bash-ts-mode)       ; No file patterns, just remapping
    (javascript-mode  js-ts-mode          "\\.js\\'" "\\.mjs\\'")
    (js2-mode         js-ts-mode)         ; No file patterns, just remapping
    (typescript-mode  typescript-ts-mode  "\\.ts\\'")
    (typescript-mode  tsx-ts-mode         "\\.tsx\\'")  ; Special case for TSX
    (css-mode         css-ts-mode         "\\.css\\'")
    (json-mode        json-ts-mode        "\\.json\\'")
    (conf-toml-mode   toml-ts-mode        "\\.toml\\'")
    (go-mode          go-ts-mode          "\\.go\\'")
    (python-mode      python-ts-mode      "\\.py\\'")
    (ruby-mode        ruby-ts-mode        "\\.rb\\'")
    (java-mode        java-ts-mode        "\\.java\\'")
    (rust-mode        rust-ts-mode        "\\.rs\\'")
    (yaml-mode        yaml-ts-mode        "\\.ya?ml\\'" "\\.yaml\\'" "\\.yml\\'"))
  "List of mode remappings and file associations for built-in tree-sitter modes.")

;; Apply all the remappings and file associations for built-in modes
(dolist (entry my/ts-mode-alist)
  (let ((from-mode (nth 0 entry))
        (to-mode (nth 1 entry))
        (patterns (cddr entry)))
    (when (fboundp to-mode)  ; Only for modes that exist at startup
      ;; Remap the mode
      (add-to-list 'major-mode-remap-alist (cons from-mode to-mode))
      ;; Add file associations
      (dolist (pattern patterns)
        (add-to-list 'auto-mode-alist (cons pattern to-mode))))))

;; Auto pairing in programming buffers.
(add-hook 'prog-mode-hook #'electric-pair-mode)

;; --- Markdown (no first-class built-in TS yet) -------------------------------
;; Keep markdown-mode for full features.
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :hook (markdown-mode . visual-line-mode))

;; --- YAML: fallback if tree-sitter mode not available -----------------------
(use-package yaml-mode
  :ensure t
  :unless (fboundp 'yaml-ts-mode)
  :mode "\\.ya?ml\\'")

;; --- Nix TS (external package) -----------------------------------------------
(use-package nix-ts-mode
  :ensure t
  :mode "\\.nix\\'"  ; Need explicit mode since package loads after our alist setup
  :config
  ;; Also set up remapping when package loads
  (add-to-list 'major-mode-remap-alist '(nix-mode . nix-ts-mode)))

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
