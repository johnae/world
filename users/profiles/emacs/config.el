;; Startup time measurement
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (emacs-init-time)
                     gcs-done)))

(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(setq-default inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq initial-buffer-choice t)
(setq inhibit-startup-buffer-menu "")

(setopt initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
(setopt display-time-default-load-average nil) ; this information is useless for most

;; Allows us to call git commit / jj commit / split etc from a vterm inside emacs
(setq server-window 'switch-to-buffer)

;; Automatically reread from disk if the underlying file changes
(setopt auto-revert-avoid-polling t)
;; Some systems don't do file notifications well; see
;; https://todo.sr.ht/~ashton314/emacs-bedrock/11
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; Save history of minibuffer
(savehist-mode)

;; Move through windows with Ctrl-<arrow keys>
(windmove-default-keybindings 'control) ; You can use other modifiers here

;; Fix archaic defaults
(setopt sentence-end-double-space nil)

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

;; Don't auto-backup
(setq make-backup-files nil)
;; Don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backup or wherever
;; (defun bedrock--backup-file-name (fpath)
;;   "Return a new file path of a given file path.
;; If the new path's directories does not exist, create them."
;;   (let* ((backupRootDir "~/.emacs.d/emacs-backup/")
;;          (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
;;          (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
;;     (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
;;     backupFilePath))
;; (setopt make-backup-file-name-function 'bedrock--backup-file-name)

;; No lock files
(setq create-lockfiles nil)

;; No auto save
(setq auto-save-default nil)

(setq select-enable-primary t)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t t)

(setq find-program "@find-program@")
(setq shell-file-name "@shell-file-name@")
(setq explicit-shell-file-name shell-file-name)

;; Enable winner mode after init
(add-hook 'after-init-hook #'winner-mode)

;; which-key: shows a popup of available keybindings when typing a long key
;; sequence (e.g. C-x ...)
(use-package which-key
  :ensure t
  :defer 2  ; Load after 2 seconds - not needed immediately
  :config
  (which-key-mode))

;; For help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion

(setopt enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
(setopt completion-cycle-threshold 1)                  ; TAB cycles candidates
(setopt completions-detailed t)                        ; Show annotations
(setopt tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
(setopt completion-styles '(basic initials substring)) ; Different styles to match input to candidates

(setopt completion-auto-help 'always)                  ; Open completion always; `lazy' another option
(setopt completions-max-height 20)                     ; This is arbitrary
(setopt completions-detailed t)
(setopt completions-format 'one-column)
(setopt completions-group t)
(setopt completion-auto-select 'second-tab)            ; Much more eager
					;(setopt completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

;; For a fancier built-in completion option, try ido-mode,
;; icomplete-vertical, or fido-mode. See also the file extras/base.el

;(icomplete-vertical-mode)
;(fido-vertical-mode)
;(setopt icomplete-delay-completions-threshold 4000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Interface enhancements/defaults
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mode line information
(setopt line-number-mode t)                        ; Show current line in modeline
(setopt column-number-mode t)                      ; Show column as well

(setopt x-underline-at-descent-line nil)           ; Prettier underlines
(setopt switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent

(setopt show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
(setopt indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

;; Enable horizontal scrolling
(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)

(setq display-line-numbers-type 'relative) 
(global-display-line-numbers-mode)

;; Disable them in certain major modes
(dolist (mode '(org-mode
                eshell-mode
                term-mode
                shell-mode
                vterm-mode
                help-mode
                compilation-mode))
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            (lambda () (display-line-numbers-mode 0))))

;; We won't set these, but they're good to know about
;;
;; (setopt indent-tabs-mode nil)
;; (setopt tab-width 4)

;; Misc. UI tweaks
(blink-cursor-mode -1)                                ; Steady cursor
(pixel-scroll-precision-mode)                         ; Smooth scrolling

;; Use common keystrokes by default
(cua-mode)

;; Display line numbers in programming mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setopt display-line-numbers-width 3)           ; Set a minimum width

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)

;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Tab-bar configuration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show the tab-bar as soon as tab-bar functions are invoked
(setopt tab-bar-show 1)

;; Add the time to the tab-bar, if visible
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setopt display-time-format "%a %F %T")
(setopt display-time-interval 1)
(display-time-mode)

;; Theme loading is handled by extras/catppuccin.el
;; (use-package emacs
;;   :config
;;   (load-theme 'modus-vivendi))          ; for light theme, use modus-operandi

@extras@

; (load-file (expand-file-name "extras/base.el" user-emacs-directory))
; (load-file (expand-file-name "extras/nomenubar.el" user-emacs-directory))
; (load-file (expand-file-name "extras/tabspaces.el" user-emacs-directory))
; (load-file (expand-file-name "extras/envrc.el" user-emacs-directory))
; (load-file (expand-file-name "extras/eat.el" user-emacs-directory))
; (load-file (expand-file-name "extras/catpuccin.el" user-emacs-directory))
; (load-file (expand-file-name "extras/font.el" user-emacs-directory))
; (load-file (expand-file-name "extras/meow.el" user-emacs-directory))
; (load-file (expand-file-name "extras/eshell.el" user-emacs-directory))
; (load-file (expand-file-name "extras/claude.el" user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("24b6ade0e3cabdfee9fa487961b089d059e048d77fe13137ea4788c1b62bd99d"
     default))
 '(package-selected-packages
   '(avy cape consult corfu corfu-terminal eat embark embark-consult
	 envrc eshell-prompt-extras json-mode kind-icon magit
	 marginalia multiple-cursors orderless tabspaces vertico vterm
	 wgrep which-key yaml yaml-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
