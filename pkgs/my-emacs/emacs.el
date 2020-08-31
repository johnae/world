;; Keep a ref to the actual file-name-handler
(defvar file-name-handler-alist-actual file-name-handler-alist)

;; Set the file-name-handler to nil (because regexing is cpu intensive)
(setq file-name-handler-alist nil)

;; Set this much higher for better performance
(setq gc-cons-threshold 100000000)

;; The default is very low - 4k, lsp responses are easily 1+ MB
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Reset file-name-handler-alist after initialization
(add-hook 'after-init-hook
  (lambda ()
    (garbage-collect)
    (setq file-name-handler-alist file-name-handler-alist-actual)) t)

;; Default to using bash as shell
(setq shell-file-name "bash")

;; Yeah it's dangerous. I like living on the edge.
(setq org-confirm-babel-evaluate nil)

;; This makes emacsclient fast when starting up
(setq-default xterm-query-timeout nil)

;; Make unpure packages archives unavailable
(setq package-archives nil)
(setq package-enable-at-startup nil)

(require 'cl-lib)

;; Initialize [[https://github.com/jwiegley/use-package][use-package]].
(eval-and-compile
  (require 'package)
  (package-initialize)
  (require 'use-package))

;; Add path to notmuch to load-path.
(add-to-list 'load-path "@NOTMUCH_LOAD_PATH@")

;; Setup auth sources to use pass gpg files.

(setq auth-sources
    '((:source "~/.local/share/password-store/emacs/auth/authinfo.gpg")))

;; The famous [[https://orgmode.org/][org mode]]. Default settings I use and stuff.

(use-package org
  :defer t
  :bind (:map org-mode-map
              ("C-c e" . org-edit-src-code))
  :init
  (setq org-log-done 'time
        org-log-reschedule 'time
        org-crypt-key "0x45FEBADDA16B8E55"
        org-src-fontify-natively t
        org-ellipsis " ↘"
        org-agenda-files '("~/Sync/org/")
        org-directory '("~/Sync/org/")
        org-enforce-todo-dependencies t
        org-startup-with-beamer-mode t
        org-export-coding-system 'utf-8
        org-agenda-sorting-strategy
         (quote
          ((agenda deadline-up priority-down)
           (todo priority-down category-keep)
           (tags priority-down category-keep)
           (search category-keep)))
        org-todo-keywords
         '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED"))
        org-capture-templates
        '(
          ("a" "My TODO task format."
          entry (file "~/Sync/org/todos.org")
          "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")

          ("r" "Email REPLY task format."
          entry (file "~/Sync/org/reply.org")
          "* TODO Respond to %:from on %:subject  :email: \nSCHEDULED: %t\n%U\n%a\n"
          :clock-in t
          :clock-resume t
          :immediate-finish t
          )))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (emacs-lisp . t)
     (ruby . t)
     (python . t)
     (js . t)
     (java . t)
     (latex . t)
     (haskell . t)
     (clojure . t)
     ;;(go . t)
     (shell . t)
     (sql . t)
     (sqlite . t)
     (groovy . t)
;;(kotlin . t)
;;(typescript . t)
     (calc . t)
     (C . t)))
  (add-hook 'org-mode-hook 'auto-revert-mode))


;;Showing org bullets as utf8 characters seemed like a cool thing.

(use-package org-bullets
  :init
  (setq org-bullets-bullet-list '("◉"))
  (add-hook 'org-mode-hook 'org-bullets-mode))


;;Presentation minor mode for org-mode see [[https://github.com/takaxp/org-tree-slide][org-tree-slide]]
(use-package org-tree-slide
  :defer t
  :config
  (define-key org-tree-slide-mode-map (kbd "<f9>") 'org-tree-slide-move-previous-tree)
  (define-key org-tree-slide-mode-map (kbd "<f10>") 'org-tree-slide-move-next-tree)
  )


;; Notifications for org TODO:s. See [[https://github.com/akhramov/org-wild-notifier.el][org-wild-notifier]]
;;(use-package org-wild-notifier
;;  :config
;;  (org-wild-notifier-mode)
;;  )


;; In a terminal, TAB corresponds to C-i so that's an issue. For the GUI emacs, which I
;; normally use, this isn't a problem.

(setq evil-want-keybinding nil)
;; see https://github.com/emacs-evil/evil-collection/issues/60
(setq evil-want-C-i-jump nil)


;; So [[https://github.com/emacs-evil/evil][evil]] is vim for emacs. A better vim basically ;-).

(use-package evil
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd ", <right>") 'split-window-horizontally)
  (define-key evil-normal-state-map (kbd ", <SPC>") 'ivy-switch-buffer)
  (define-key evil-normal-state-map (kbd ", p") 'counsel-projectile-find-file)
  (define-key evil-normal-state-map (kbd ", f") 'counsel-find-file)
  (define-key evil-normal-state-map (kbd ", s") 'swiper)
  (define-key evil-normal-state-map (kbd ", e") 'jae/eshell-here)
  (define-key evil-normal-state-map (kbd ", n") 'jae/eshell-new)
  (define-key evil-normal-state-map (kbd ", a") 'counsel-projectile-rg)
  (define-key evil-normal-state-map (kbd ", <up>") 'projectile-switch-project)
  (define-key evil-normal-state-map (kbd "P") 'counsel-yank-pop)
  (define-key evil-normal-state-map (kbd ", <down>") 'split-window-vertically)
  (define-key evil-normal-state-map (kbd ", g") 'magit-status)
  (define-key evil-normal-state-map (kbd ", w") 'whitespace-cleanup)
  (define-key evil-normal-state-map (kbd ", b") 'previous-buffer)
  (define-key evil-normal-state-map (kbd ", f") 'next-buffer)
  (define-key evil-normal-state-map (kbd ", B") 'backward-page)
  (define-key evil-normal-state-map (kbd ", F") 'forward-page)
  (define-key evil-normal-state-map (kbd ", +") 'enlarge-window)
  (define-key evil-normal-state-map (kbd ", -") 'shrink-window)
  (define-key evil-normal-state-map (kbd ", <RET>") 'projectile-ag))


;; [[https://github.com/redguardtoo/evil-nerd-commenter][Evil nerd commenter]] let's me comment out one of more lines. In Evil mode this is done using
;; C-c i (multiple lines by selecting a region). A reimplementation of the vim version.

(use-package evil-nerd-commenter
  :config
  (evilnc-default-hotkeys))


;; [[https://github.com/redguardtoo/evil-nerd-commenter][Evil surround]] makes it easy to surround text in say quotes or parens.
;; See: https://github.com/emacs-evil/evil-surround

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))


;; Evil keybindings for [[https://orgmode.org/][org-mode]]. See [[https://github.com/Somelauw/evil-org-mode][https://github.com/Somelauw/evil-org-mode]]. Yay.

(use-package evil-org
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme))))


;; Evil keybindings for many things.
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))


;; Evil keybindings for magit.
(use-package evil-magit)


;; Polymode allows several major modes in one buffer.
(use-package polymode)

(use-package poly-org
  :config
  (add-hook 'org-mode-hook 'poly-org-mode)
)

(use-package poly-markdown
  :config
  (add-hook 'markdown-mode-hook 'poly-markdown-mode)
)


;; Avy is kind of like vim motion. It lets you jump to certain text using a
;; character based decision tree.
;; See: [[https://github.com/abo-abo/avy][https://github.com/abo-abo/avy]]

(use-package avy
  :config
  (global-set-key (kbd "C-c ¨") 'avy-goto-char)
)


;; Which key will show (in a popup) any possible continuations of a currently entered incomplete command.
;; See: [[https://github.com/justbur/emacs-which-key][https://github.com/justbur/emacs-which-key]]
(use-package which-key
  :diminish (which-key-mode . "")
  :init
  (which-key-mode)
  :config
  (which-key-setup-side-window-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-side-window-max-width 0.33
        which-key-idle-delay 0.05)
  )


;; Completion tools.
;; See: [[https://github.com/abo-abo/swiper][https://github.com/abo-abo/swiper]]
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d ")
  (add-hook 'shell-mode-hook '(lambda ()
    (define-key shell-mode-map "\t" 'completion-at-point))))

(setq ivy-do-completion-in-region t)
(defun setup-eshell-ivy-completion ()
  (define-key eshell-mode-map [remap eshell-pcomplete] 'completion-at-point))
  ;;(setq-local ivy-display-functions-alist
  ;;            (remq (assoc 'ivy-completion-in-region ivy-display-functions-alist)
  ;;                  ivy-display-functions-alist)))

(add-hook 'eshell-mode-hook #'setup-eshell-ivy-completion)


;;Counsel integration for projectile.

;;See: [[https://github.com/ericdanan/counsel-projectile][https://github.com/ericdanan/counsel-projectile]]

(use-package counsel-projectile
  :diminish (projectile-mode . "")
  :config
  (projectile-mode)
  (counsel-projectile-mode))

;; This will name buffers with the project relative path to the file name rather than
;; just the file name. Useful in larger projects.
(defun my-project-relative-buffer-name ()
  (ignore-errors
    (rename-buffer
     (file-relative-name buffer-file-name (projectile-project-root)))))

(add-hook 'find-file-hook #'my-project-relative-buffer-name)


;; Sorting and filtering for company and ivy.
;; See: [[https://github.com/raxod502/prescient.el][https://github.com/raxod502/prescient.el]]

(use-package prescient
   :config
   (prescient-persist-mode))
(use-package ivy-prescient
   :config
   (ivy-prescient-mode))
(use-package company-prescient
   :config
   (company-prescient-mode))

;; See: [[https://www.emacswiki.org/emacs/PosTip][https://www.emacswiki.org/emacs/PosTip]]
;; get tooltips at point
(use-package pos-tip)
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)


;; For editing html, css etc.
;; See: [[https://github.com/fxbois/web-mode][https://github.com/fxbois/web-mode]]

(use-package web-mode
  :mode "\\.html?$")


;; You know, for docker.
(use-package dockerfile-mode
  :mode "Dockerfile.*")


;; HashiCorps terraform.
(use-package terraform-mode
  :mode "\\.tf$"
  :init
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))


(use-package company-terraform
  :after company
  :init
  (company-terraform-init))

;;For editing nix expressions.
(defun my-nix-mode-format-before-save-hook ()
  (when (eq major-mode 'nix-mode)
    (nix-format-buffer)))

(use-package nix-mode
  :mode "\\.nix\\'"
  :config
  (add-hook 'before-save-hook #'my-nix-mode-format-before-save-hook))

;; [[https://magit.vc/][Magit]] is possibly the most awesome git integration of any editor out there.
(use-package magit
  :config
  (setq magit-auto-revert-mode nil)
  (setq magit-repository-directories
        '( "~/Development" ))
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  )


;; Because in evil mode I often want to go to a line x lines below and therefore I want to see those
;; numbers in the fringe. I'm still interested in the current line number though so I want that to show
;; for the line that I'm on.

;; relative line numbers
(use-package linum-relative
  :config
  (setq linum-relative-format "%s")
  (setq linum-relative-current-symbol "")
  (add-hook 'prog-mode-hook #'linum-mode)
  (linum-relative-mode t))


;; Helps with the fringe? :-)
(use-package fringe-helper
  :init
  (setq-default left-fringe-width  16)
  (setq-default right-fringe-width 16)
  :config
  )


;; Direnv integration for emacs.
;; See: [[https://github.com/wbolster/emacs-direnv][https://github.com/wbolster/emacs-direnv]]
;; and ofc
;; https://direnv.net/]]
(use-package direnv
  :init
  (add-hook 'prog-mode-hook #'direnv-update-environment)
  :config
  (direnv-mode)
  (add-to-list 'direnv-non-file-modes 'eshell-mode)
)


;; Highlights uncommitted changes.
(use-package diff-hl
  :config
  (setq diff-hl-side 'right)
  (global-diff-hl-mode t)
  (diff-hl-flydiff-mode)
  (add-hook 'after-make-frame-functions(lambda (frame)
    (if (window-system frame)
      (diff-hl-mode)
      (diff-hl-margin-mode))))
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

;; Some simple modes for a few languages.
(use-package moonscript
  :mode ("\\Spookfile.*\\'" . moonscript-mode))

(use-package lua-mode)

(use-package json-mode
  :mode (("\\.bowerrc$" . json-mode)
     ("\\.jshintrc$" . json-mode)
     ("\\.json_schema$" . json-mode)
     ("\\.json\\'" . json-mode))
  :bind (:package json-mode-map
     :map json-mode-map
         ("C-c <tab>" . json-mode-beautify))
  :config
  (make-local-variable 'js-indent-level))

(use-package yaml-mode
  :mode "\\.cf$")

(use-package js2-mode
  :config
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-missing-semi-one-line-override t)
  (setq js-indent-level 2)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package rust-mode
  :mode "\\.rs$"
  :config
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
)

;;(defun my-reset-cargo-custom-path ()
;;  "Always reevaluate cargo bin path (so direnv + nix works nicely)."
;;  (setq cargo-process--custom-path-to-bin (executable-find "cargo"))
;;)
;;(add-hook 'post-command-hook 'my-reset-cargo-custom-path)

(use-package swift-mode)

;; Fix executable-find so it uses direnvs environment.

;; (defun my-executable-find (orig-fun &rest args)
;;   (direnv-update-environment default-directory)
;;   (apply orig-fun args))
;;
;; (advice-add 'executable-find :around #'my-executable-find)

;; Language Server Protocol.
;; See: https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :after (direnv evil)
  :init
  (setq lsp-keymap-prefix "C-c a")
  (setq lsp-prefer-capf t)
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-snippet nil)
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                    :major-modes '(nix-mode)
                    :server-id 'nix))
  (setq lsp-file-watch-ignored '(
    "[/\\\\]\\.direnv$"
    ; SCM tools
    "[/\\\\]\\.git$"
    "[/\\\\]\\.hg$"
    "[/\\\\]\\.bzr$"
    "[/\\\\]_darcs$"
    "[/\\\\]\\.svn$"
    "[/\\\\]_FOSSIL_$"
    ; IDE tools
    "[/\\\\]\\.idea$"
    "[/\\\\]\\.ensime_cache$"
    "[/\\\\]\\.eunit$"
    "[/\\\\]node_modules$"
    "[/\\\\]\\.fslckout$"
    "[/\\\\]\\.tox$"
    "[/\\\\]\\.stack-work$"
    "[/\\\\]\\.bloop$"
    "[/\\\\]\\.metals$"
    "[/\\\\]target$"
    ; Autotools output
    "[/\\\\]\\.deps$"
    "[/\\\\]build-aux$"
    "[/\\\\]autom4te.cache$"
    "[/\\\\]\\.reference$"))
  :hook (
    (lsp-mode . lsp-enable-which-key-integration))
 )

(defun my-lsp ()
  "Ensures environment is updated before enabling lsp mode."
  (direnv-update-environment default-directory)
  (lsp-deferred))

(add-hook 'prog-mode-hook 'my-lsp)

(use-package lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-doc-max-height 60)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
)

(use-package company-lsp
  :after lsp-mode
  :config
  (push 'company-lsp company-backends)
  :defer t
)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)


;; Flycheck rust enables syntax checking.
(use-package flycheck-rust
  :after (rust-mode flycheck)
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))


;; Prettier for js/typescript etc code formatting.
(use-package prettier-js
  :config
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'typescript-mode-hook 'prettier-js-mode))


;; Company]] is a text completion framework for Emacs. The name stands for "complete anything". It uses pluggable back-ends
;; and front-ends to retrieve and display completion candidates.
;;
;; It comes with several back-ends such as Elisp, Clang, Semantic, Eclim, Ropemacs, Ispell, CMake, BBDB, Yasnippet, dabbrev,
;; etags, gtags, files, keywords and a few others.
(use-package company
  :diminish (company-mode . "")
  :init
  (setq company-idle-delay 0
        company-echo-delay 0
        ;company-begin-commands '(self-insert-command)
        company-minimum-prefix-length 2
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)
  :config
  (global-company-mode))


;; Show documentation popups when idling on a completion candidate.
;; See: [[https://github.com/expez/company-quickhelp][https://github.com/expez/company-quickhelp]]

(use-package company-quickhelp
  :config
  (company-quickhelp-mode 1)
  (setq company-quickhelp-delay 0))

;; Show documentation popups for nixos configuration options.
(use-package company-nixos-options
  :defer t
  :init
  :after (company)
  ;;(with-eval-after-load 'company
  ;;  (add-to-list 'company-backends 'company-nixos-options))
  )

;; This allows me to toggle between snake case, camel case etc.
(use-package string-inflection
  :config
  (global-set-key (kbd "C-c i") 'string-inflection-cycle)
  (global-set-key (kbd "C-c C") 'string-inflection-camelcase)        ;; Force to CamelCase
  (global-set-key (kbd "C-c L") 'string-inflection-lower-camelcase)  ;; Force to lowerCamelCase
  (global-set-key (kbd "C-c J") 'string-inflection-java-style-cycle) ;; Cycle through Java styles
  )


;; Flycheck is "Syntax checking for emacs".
(use-package flycheck
  :config
  (global-flycheck-mode)
  (setq flycheck-idle-change-delay 2.0)
  ;; (setq flycheck-check-syntax-automatically '(mode-enabled save))
  ;; since we wrapped executable-find this will run direnv-update-environment before finding executable
  (setq flycheck-executable-find
        (lambda (cmd) (executable-find cmd)))
)


;; Go mode and other go stuff.
(use-package go-mode
  :config
  (setq gofmt-command "goimports")
)
(add-hook 'before-save-hook 'gofmt-before-save)

(use-package go-guru
  :config
  (go-guru-hl-identifier-mode))

(use-package flycheck-gometalinter
  :after flycheck
  :config
  (setq flycheck-gometalinter-fast t
        flycheck-gometalinter-test t
        flycheck-gometalinter-deadlines "10s")
  (progn
    (flycheck-gometalinter-setup)))

(use-package go-eldoc
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))


;; For showing errors in terminal (pos-tip doesn't do that - see below).
;; See: [[https://github.com/flycheck/flycheck-popup-tip][https://github.com/flycheck/flycheck-popup-tip]]
(use-package flycheck-popup-tip)

;; For showing errors under point. Refers to above for similar terminal functionality.
;; See: [[https://github.com/flycheck/flycheck-pos-tip][https://github.com/flycheck/flycheck-pos-tip]]
(use-package flycheck-pos-tip
  :config
  (setq flycheck-pos-tip-display-errors-tty-function #'flycheck-popup-tip-show-popup)
  (setq flycheck-pos-tip-timeout 0)
  (flycheck-pos-tip-mode))

;; Check those bashisms. Posix ftw!
(use-package flycheck-checkbashisms
  :config
  (flycheck-checkbashisms-setup))


;; When programming I like to see clearly which line I'm editing atm.
(add-hook 'prog-mode-hook 'hl-line-mode)

;; This will highlight matching parentheses. Some additional configuration for that.
(defun my-show-paren-mode ()
   "Enables show-paren-mode."
   (setq show-paren-delay 0)
   (set-face-background 'show-paren-match (face-background 'default))
   (set-face-foreground 'show-paren-match "#def")
   (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
   (show-paren-mode 1))
(add-hook 'prog-mode-hook 'my-show-paren-mode)

;; Electric pair-mode will help with matching parentheses, quotes etc. Only used for prog mode.
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; Sometimes I edit markdown.
(use-package markdown-mode)


;; Highlights numbers in source code.
;; See: [[https://github.com/Fanael/highlight-numbers][https://github.com/Fanael/highlight-numbers]]
(use-package highlight-numbers
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))


;; UndoTree let's me visualize the past state of a buffer.
;; See: [[https://www.emacswiki.org/emacs/UndoTree][https://www.emacswiki.org/emacs/UndoTree]]
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (define-key evil-normal-state-map (kbd "U") 'undo-tree-visualize)
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-diff t))


;; Frames only mode makes emacs play nicely with tiling window managers (such as i3). It uses
;; new operating system windows instead of emacs internal ones.
;; See: [[https://github.com/davidshepherd7/frames-only-mode][https://github.com/davidshepherd7/frames-only-mode]]
(use-package frames-only-mode
  :config
  (frames-only-mode))


;; Using control-x control-z to zoom in / out a window (eg. "fullscreen" it).
(use-package zoom-window
  :bind* ("C-x C-z" . zoom-window-zoom))


;; Highlight the part of a line that goes beyond 100 chars
(use-package column-enforce-mode
  :config
  (setq column-enforce-comments nil)
  (setq column-enforce-column 100)
  (add-hook 'prog-mode-hook 'column-enforce-mode))


;; Alerts. Using for example libnotify on Linux.
(use-package alert
  :custom (alert-default-style 'libnotify))


(with-eval-after-load 'message
  (setq message-cite-style message-cite-style-gmail)
  (setq message-citation-line-function 'message-insert-formatted-citation-line)
  (setq message-citation-line-format "On %a, %b %e, %Y at %I:%M %p %f wrote:"))

(use-package messages-are-flowing
  :ensure t
  :hook ((message-mode . messages-are-flowing-use-and-mark-hard-newlines)))


;; The amazing notmuch email client.
(require 'notmuch)
(require 'ol-notmuch)

(setq user-mail-address (notmuch-user-primary-email)
      notmuch-mail-dir (expand-file-name ".mail" (getenv "HOME"))
      user-full-name (notmuch-user-name)
      message-kill-buffers-on-exit t
      sendmail-program "gmi"
      send-mail-function 'sendmail-send-it
      message-sendmail-extra-arguments '("send" "--quiet" "-t" "-C" "~/.mail/personal")
      message-sendmail-f-is-evil t

      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header

      notmuch-always-prompt-for-sender t
      notmuch-archive-tags '("-inbox" "-unread")
      notmuch-show-mark-read-tags '("-inbox" "-unread")
      notmuch-search-oldest-first nil
      notmuch-show-indent-content nil
      notmuch-fcc-dirs nil
      notmuch-hooks-dir (expand-file-name ".notmuch/hooks" notmuch-mail-dir))

(defun insane/notmuch-set-account ()
  (if (message-mail-p)
      (save-excursion
  (let* ((from (save-restriction
           (message-narrow-to-headers)
           (message-fetch-field "from")))
         (account (cond
       ((string-match "john@insane.se" from) "~/.mail/personal")
       ((string-match "john@instabox.se" from) "~/.mail/professional")
       ((string-match "john@instabox.io" from) "~/.mail/professional")
       )))
    (progn
            (setq message-sendmail-extra-arguments (list '"send" '"--quiet" '"-t" '"-C" account))
            )
    ))))
(add-hook 'message-send-mail-hook 'insane/notmuch-set-account)

(progn
  (setq notmuch-saved-searches nil)
  (push '(:name "Inbox"
                :query "tag:inbox AND tag:screened AND tag:unread"
                :key "i"
                :search-type 'tree)
        notmuch-saved-searches)
  (push '(:name "Previously Seen"
                :query "tag:screened AND NOT tag:unread"
                :key "I")
        notmuch-saved-searches)
  (push '(:name "Unscreened"
                :query "tag:inbox AND NOT tag:screened"
                :key "s")
        notmuch-saved-searches)
  (push '(:name "The Feed"
                :query "tag:thefeed"
                :key "f"
                :search-type 'tree)
        notmuch-saved-searches)
  (push '(:name "The Papertrail"
                :query "tag:/ledger/"
                :key "p")
        notmuch-saved-searches))


(eval-after-load 'notmuch-show
  (progn
     ;; Bindings in `notmuch-show-mode'
    (evil-define-key 'normal notmuch-show-mode-map
      (kbd "r") 'notmuch-show-reply
      (kbd "R") 'notmuch-show-reply-sender
      (kbd "C") 'insane/notmuch-reply-later
    )

     ;; Bindings in `notmuch-search-mode'
    (evil-define-key 'normal notmuch-search-mode-map
      (kbd "r") 'notmuch-search-reply-to-thread
      (kbd "R") 'notmuch-search-reply-to-thread-sender
      (kbd "/") 'notmuch-search-filter
      (kbd "A") 'insane/notmuch-archive-all
      (kbd "D") 'insane/notmuch-delete-all
      (kbd "L") 'insane/notmuch-filter-by-from
      (kbd ";") 'insane/notmuch-search-by-from
      (kbd "d") 'insane/notmuch-search-delete-and-archive-thread
      ;; Hey wf
      (kbd "S") 'insane/notmuch-move-sender-to-spam
      (kbd "I") 'insane/notmuch-move-sender-to-screened
      (kbd "P") 'insane/notmuch-move-sender-to-papertrail
      (kbd "f") 'insane/notmuch-move-sender-to-thefeed
      (kbd "C") 'insane/notmuch-reply-later
    )

     ;; Bindings in `notmuch-tree-mode'
    (evil-define-key 'normal notmuch-tree-mode-map
      (kbd "C") 'insane/notmuch-reply-later
    )
))

(defun insane/notmuch-archive-all ()
  "Archive all the emails in the current view."
  (interactive)
  (notmuch-search-archive-thread nil (point-min) (point-max)))

(defun insane/notmuch-delete-all ()
  "Archive all the emails in the current view.
Mark them for deletion by cron job."
  (interactive)
  (notmuch-search-tag-all '("+deleted"))
  (insane/notmuch-archive-all))

(defun insane/notmuch-search-delete-and-archive-thread ()
  "Archive the currently selected thread. Add the deleted tag as well."
  (interactive)
  (notmuch-search-add-tag '("+deleted"))
  (notmuch-search-archive-thread))

(defun insane/notmuch-tag-and-archive (tag-changes &optional beg end)
  "Prompt the user for TAG-CHANGES.
Apply the TAG-CHANGES to region and also archive all the emails.
When called directly, BEG and END provide the region."
  (interactive (notmuch-search-interactive-tag-changes))
  (notmuch-search-tag tag-changes beg end)
  (notmuch-search-archive-thread nil beg end))

(defun insane/notmuch-search-get-from ()
  "A helper function to find the email address for the given email."
  (let ((notmuch-addr-sexp (car
                            (notmuch-call-notmuch-sexp "address"
                                                       "--format=sexp"
                                                       "--format-version=1"
                                                       "--output=sender"
                                                       (notmuch-search-find-thread-id)))))
    (plist-get notmuch-addr-sexp :name-addr)))

(defun insane/notmuch-tree-get-from ()
  "A helper function to find the email address for the given email.
Assumes `notmuch-tree-mode'."
  (plist-get (notmuch-tree-get-prop :headers) :From))

(defun insane/notmuch-get-from ()
  "Find the From email address for the email at point."
  (car (notmuch-clean-address (cond
                               ((eq major-mode 'notmuch-show-mode)
                                (notmuch-show-get-from))
                               ((eq major-mode 'notmuch-tree-mode)
                                (insane/notmuch-tree-get-from))
                               ((eq major-mode 'notmuch-search-mode)
                                (insane/notmuch-search-get-from))
                               ((t nil))))))

(defun insane/notmuch-filter-by-from ()
  "Filter the current search view to show all emails sent from the sender of the current thread."
  (interactive)
  (notmuch-search-filter (concat "from:" (insane/notmuch-get-from))))

(defun insane/notmuch-search-by-from (&optional no-display)
  "Show all emails sent from the sender of the current thread.
NO-DISPLAY is sent forward to `notmuch-search'."
  (interactive)
  (notmuch-search (concat "from:" (insane/notmuch-get-from))
                  notmuch-search-oldest-first
                  nil
                  nil
                  no-display))

(defun insane/notmuch-tag-by-from (tag-changes &optional beg end refresh)
  "Apply TAG-CHANGES to all emails from the sender of the current thread.
BEG and END provide the region, but are ignored. They are defined
since `notmuch-search-interactive-tag-changes' returns them. If
REFRESH is true, refresh the buffer from which we started the
search."
  (interactive (notmuch-search-interactive-tag-changes))
  (let ((this-buf (current-buffer)))
    (insane/notmuch-search-by-from t)
    ;; This is a dirty hack since I can't find a way to run a
    ;; temporary hook on `notmuch-search' completion. So instead of
    ;; waiting on the search to complete in the background and then
    ;; making tag-changes on it, I will just sleep for a short amount
    ;; of time. This is generally good enough and works, but is not
    ;; guaranteed to work every time. I'm fine with this.
    (sleep-for 0.5)
    (notmuch-search-tag-all tag-changes)
    (when refresh
      (set-buffer this-buf)
      (notmuch-refresh-this-buffer))))

(defun insane/notmuch-add-addr-to-db (nmaddr nmdbfile)
  "Add the email address NMADDR to the db-file NMDBFILE."
  (append-to-file (format "%s\n" nmaddr) nil nmdbfile))

(defun insane/notmuch-move-sender-to-thefeed ()
  "For the email at point, move the sender of that email to the feed.
This means:
1. All new email should go to the feed and skip the inbox altogether.
2. All existing email should be updated with the tag =thefeed=.
3. All existing email should be removed from the inbox."
  (interactive)
  (insane/notmuch-add-addr-to-db (insane/notmuch-get-from)
                                 (format "%s/thefeed.db" notmuch-hooks-dir))
  (insane/notmuch-tag-by-from '("+thefeed" "-inbox")))

(defun insane/notmuch-move-sender-to-papertrail (tag-name)
  "For the email at point, move the sender of that email to the papertrail.
This means:
1. All new email should go to the papertrail and skip the inbox altogether.
2. All existing email should be updated with the tag =ledger/TAG-NAME=.
3. All existing email should be removed from the inbox."
  (interactive "sTag Name: ")
  (insane/notmuch-add-addr-to-db (format "%s %s"
                                         tag-name
                                         (insane/notmuch-get-from))
                                 (format "%s/ledger.db" notmuch-hooks-dir))
  (let ((tag-string (format "+ledger/%s" tag-name)))
    (insane/notmuch-tag-by-from (list tag-string "-inbox" "-unread"))))

(defun insane/notmuch-move-sender-to-screened ()
  "For the email at point, move the sender of that email to Screened Emails.
This means:
1. All new email should be tagged =screened= and show up in the inbox.
2. All existing email should be updated to add the tag =screened=."
  (interactive)
  (insane/notmuch-add-addr-to-db (insane/notmuch-get-from)
                                 (format "%s/screened.db" notmuch-hooks-dir))
  (insane/notmuch-tag-by-from '("+screened")))

(defun insane/notmuch-move-sender-to-spam ()
  "For the email at point, move the sender of that email to spam.
This means:
1. All new email should go to =spam= and skip the inbox altogether.
2. All existing email should be updated with the tag =spam=.
3. All existing email should be removed from the inbox."
  (interactive)
  (insane/notmuch-add-addr-to-db (insane/notmuch-get-from)
                                 (format "%s/spam.db" notmuch-hooks-dir))
  (insane/notmuch-tag-by-from '("+spam" "+deleted" "-inbox" "-unread" "-screened")))

(defun insane/notmuch-reply-later ()
  "Capture this email for replying later."
  (interactive)
  ;; You need `org-capture' to be set up for this to work. Add this
  ;; code somewhere in your init file after `org-cature' is loaded:

  ;; (push '("r" "Respond to email"
  ;;         entry (file org-default-notes-file)
  ;;         "* TODO Respond to %:from on %:subject  :email: \nSCHEDULED: %t\n%U\n%a\n"
  ;;         :clock-in t
  ;;         :clock-resume t
  ;;         :immediate-finish t)
  ;;       org-capture-templates)

  (org-capture nil "r")

  ;; The rest of this function is just a nice message in the modeline.
  (let* ((email-subject (format "%s..."
                                (substring (notmuch-show-get-subject) 0 15)))
         (email-from (format "%s..."
                             (substring (notmuch-show-get-from) 0 15)))
         (email-string (format "%s (From: %s)" email-subject email-from)))
    (message "Noted! Reply Later: %s" email-string)))

;; Sign messages by default.
(add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)

(setq notmuch-address-selection-function
      (lambda (prompt collection initial-input)
        (completing-read prompt
                         (cons initial-input collection)
                         nil
                         t
                         nil
                         'notmuch-address-history)))

(defun disable-auto-fill ()
  "I don't want `auto-fill-mode'."
  (auto-fill-mode -1))

(add-hook 'message-mode-hook 'disable-auto-fill)

;; Visual-fill-column. Helps with composing emails.
(use-package visual-fill-column
  :ensure t)

;; Sending email.
(require 'jl-encrypt)
(setq mml-secure-insert-signature "always")

(use-package tramp
  :defer 5
  :config
  (with-eval-after-load 'tramp-cache
    (setq tramp-persistency-file-name "~/.emacs.d/tramp"))
  (setq tramp-default-method "ssh"
        tramp-default-user-alist '(("\\`su\\(do\\)?\\'" nil "root"))
        ;;tramp-use-ssh-controlmaster-options nil
        backup-enable-predicate
        (lambda (name)
          (and (normal-backup-enable-predicate name)
               (not (let ((method (file-remote-p name 'method)))
                      (when (stringp method)
                        (member method '("su" "sudo")))))))))

;; Allows memoization of expensive functions.
(use-package memoize)

;; Use wl-clipboard for interprocess copy/paste.
(setq wl-copy-process nil)
(defun wl-copy (text)
  (setq wl-copy-process (make-process :name "@WLCOPY@"
                                      :buffer nil
                                      :command '("@WLCOPY@" "-f" "-n")
                                      :connection-type 'pipe))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))
(defun wl-paste ()
  (if (and wl-copy-process (process-live-p wl-copy-process))
      nil ; should return nil if we're the current paste owner
      (shell-command-to-string "@WLPASTE@ -n | tr -d \r")))
(setq interprogram-cut-function 'wl-copy)
(setq interprogram-paste-function 'wl-paste)


;; Other configuration

;; This is the opposite of fill-paragraph.
(defun unfill-paragraph (&optional region)
  "Take a multi-line paragraph and an optional REGION and make it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
(global-set-key (kbd "M-Q") 'unfill-paragraph)

;; This will return a secret from the password store.
(defmemoize my/secret (storepath)
 "Return the contents in gnupg encrypted STOREPATH argument."
  (replace-regexp-in-string "\n\\'" ""
   (shell-command-to-string
    (concat "gpg --decrypt "
            "~/.local/share/password-store/" storepath ".gpg 2>/dev/null"))))

;; Define a function to set the telephone line theme. This is so that when using emacsclient we
;; can just call this rather than duplicate code. So we need to be able to set the theme more
;; than once depending on whether we use the emacsclient or not.
(defun my-telephone-line-theme ()
  "Enables the current telephone line theme."
  (setq telephone-line-primary-right-separator 'telephone-line-abs-left
      telephone-line-secondary-right-separator 'telephone-line-abs-hollow-left)
  (setq telephone-line-height 24
      telephone-line-evil-use-short-tag t)
  (telephone-line-mode 1))

(use-package telephone-line
  :config
  (my-telephone-line-theme))

(use-package nord-theme)

;; Define the overall theme somewhere for reuse.
(defvar my:theme 'nord)
(load-theme my:theme t)


;; This is where we recognize whether emacsclient is being used or not and if it is we'll set the theme as necessary.
(defvar my:theme-window-loaded nil)
(defvar my:theme-terminal-loaded nil)

(if (daemonp)
    (add-hook 'after-make-frame-functions(lambda (frame)
                       (select-frame frame)
                       (if (window-system frame)
                           (unless my:theme-window-loaded
                             (if my:theme-terminal-loaded
                                 (enable-theme my:theme)
                               (load-theme my:theme t)
                               (my-telephone-line-theme))
                             (setq my:theme-window-loaded t))
                         (unless my:theme-terminal-loaded
                           (if my:theme-window-loaded
                               (enable-theme my:theme)
                             (load-theme my:theme t)
                             (my-telephone-line-theme))
                           (setq my:theme-terminal-loaded t)))))

  (progn
    (load-theme my:theme t)
    (if (display-graphic-p)
        (setq my:theme-window-loaded t)
      (setq my:theme-terminal-loaded t))))

;; Capture those tasks.
(defun insane-org-task-capture ()
  "Capture a task with the default template."
  (interactive)
  (org-capture nil "a"))

(define-key global-map (kbd "C-c t") 'insane-org-task-capture)

(defun insane-things-todo ()
  "Return the default todos filepath."
  (interactive)
  (find-file (expand-file-name "~/Sync/org/todos.org")))

(define-key global-map (kbd "C-c C-t") 'insane-things-todo)

;;Define some keybindings I like for moving between splits/windows.

(global-set-key (kbd "C-<up>") 'windmove-up)
(global-set-key (kbd "C-<down>") 'windmove-down)
(global-set-key (kbd "C-<left>") 'windmove-left)
(global-set-key (kbd "C-<right>") 'windmove-right)


;; We don't want any scratch message at all. Unfortunately, because the emacs devs don't want a sysadmin
;; to disable the startup screen for users (or something like that), we can't disable that from here. Must
;; be added to a user's .emacs or init.el.

;; inhibit-startup-screen has to be in .emacs - see emacs source
;; for why
(setq initial-scratch-message "")


;; Disable some things I'm not interested in, like tool bars and menu bars.
;; No menus or anything like that thanks
(tool-bar-mode -1)
;; (scroll-bar-mode -1) ;; scrollbars are still nice though
(blink-cursor-mode -1)
(menu-bar-mode -1)


;; This is a nice font :-).
(add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font 18"))
(set-face-attribute 'default t :font "JetBrainsMono Nerd Font 18")


;; Did I mention I like utf8? I like utf8.
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; Fix the scrolling which isn't very nice by default in my opinion.
;; Sane scrolling - 1 step at a time etc
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-conservatively 10000
      scroll-step 1
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")


;; Some other general settings.
(setq mode-require-final-newline nil)
(setq tab-stop-list (number-sequence 2 120 2))
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq tabify nil)
;; Highlight trailing whitespace.
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "yellow")


;; Eshell settings and tweaks.
(use-package fish-completion
  :config
  (global-fish-completion-mode)
)

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :ensure t)

(setq eshell-buffer-maximum-lines 1200)
(defun eos/truncate-eshell-buffers ()
  "Truncates all eshell buffers."
  (interactive)
  (save-current-buffer
    (dolist (buffer (buffer-list t))
      (set-buffer buffer)
      (when (eq major-mode 'eshell-mode)
        (eshell-truncate-buffer)))))
;; After idling for 5 seconds, truncates all eshell-buffers
(setq eos/eshell-truncate-timer
      (run-with-idle-timer 5 t #'eos/truncate-eshell-buffers))

(defun eshell/clear ()
  "Really clear the eshell buffer."
  (interactive)
  (let ((input (eshell-get-old-input)))
    (eshell/clear-scrollback)
    ;;(eshell-emit-prompt)
    (insert input)))

(defun jae/eshell-new()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))

(setq eshell-where-to-jump 'begin
      eshell-review-quick-commands nil
      eshell-smart-space-goes-to-end t
      eshell-glob-case-insensitive t
      eshell-scroll-to-bottom-on-input 'this
      eshell-buffer-shorthand t
      eshell-history-size 1024
      eshell-cmpl-ignore-case t)

(require 'eshell)
(require 'em-tramp)
(setq password-cache t)
(setq password-cache-expiry 3600)
(with-eval-after-load 'esh-module
  (delq 'eshell-banner eshell-modules-list)
  (push 'eshell-tramp eshell-modules-list))
(setq eshell-modify-global-environment t)
(add-hook 'post-command-hook '(lambda ()
   (setq eshell-path-env (getenv "PATH"))
  )
)
(with-eval-after-load 'eshell
  (require 'f)

  (defun jae/eshell-here ()
    "Opens up a new shell in the directory associated with the
current buffer's file."
    (interactive)
    (let* ((height (/ (window-total-height) 3))
           (name (format "*eshell: %s*"
                         (f-filename default-directory))))
      ;;(split-window-vertically (- height))
      ;;(other-window 1)
      (let ((buf (get-buffer name)))
        (if buf
            (switch-to-buffer buf)
          (eshell 'new)
          (rename-buffer name 'unique)))
      (insert (concat "ls"))
      (eshell-send-input)))

  (require 'dash)
  (require 's)

  (defmacro with-face (STR &rest PROPS)
    "Return STR propertized with PROPS."
    `(propertize ,STR 'face (list ,@PROPS)))

  (defmacro esh-section (NAME ICON FORM &rest PROPS)
    "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
    `(setq ,NAME
           (lambda () (when ,FORM
                   (-> ,ICON
                      (concat esh-section-delim ,FORM)
                      (with-face ,@PROPS))))))

  (defun esh-acc (acc x)
    "Accumulator for evaluating and concatenating esh-sections."
    (--if-let (funcall x)
        (if (s-blank? acc)
            it
          (concat acc esh-sep it))
      acc))

  (defun shell-command-to-string-nows (cmd)
    "Return shell command output without trailing newline and whitespace."
    (replace-regexp-in-string "\n\\'" ""
      (replace-regexp-in-string "\\(\\`[[:space:]\n]*\\|[[:space:]\n]*\\'\\)" ""
        (shell-command-to-string cmd)
        )
      )
    )

  (defun is-inside-git-tree ()
    "Returns true if inside git work tree."
    (string= (shell-command-to-string-nows "git rev-parse --is-inside-work-tree 2>/dev/null")
                 "true"
                 ))

  (defun git-unpushed-commits ()
    "Returns number of local commits not pushed."
    (if (is-inside-git-tree)
        (let ((
               changes (shell-command-to-string-nows "git log @{u}.. --oneline 2> /dev/null | wc -l")
               ))
          (if (string= changes "0")
              nil
            changes
            )
          )
      nil
      )
    )

  (defun git-changes ()
    "Returns number of changes or nil."
    (if (is-inside-git-tree)
        (let ((
               diffs (shell-command-to-string-nows "git diff-index HEAD 2> /dev/null | wc -l")
               ))
          (if (string= diffs "0")
              nil
            diffs
            )
          )
      nil
      )
    )

  (defun k8s-context ()
    "Return k8s context or nil"
    (let ((
           k8s-ctx (shell-command-to-string-nows
                    "kubectl config current-context 2>/dev/null")
                   ))
      (if (string= k8s-ctx "")
          nil
        k8s-ctx
        )
      )
    )

  (defun current-gcloud-project ()
    "Returns the current gcloud project."
    (let ((
           gcloud-project (
              shell-command-to-string-nows
                 "cat ~/.config/gcloud/configurations/config_default | grep 'project =' | awk '{print $NF}'")
                          ))
      (if (string= gcloud-project "")
          nil
        gcloud-project
        )
      )
    )

  (add-hook 'eshell-first-time-mode-hook
        (lambda ()
          (define-key
            eshell-mode-map
            (kbd "C-<up>") 'windmove-up
          )
          (define-key
            eshell-mode-map
            (kbd "C-<down>") 'windmove-down
          )
          (define-key
            eshell-mode-map
            (kbd "C-<left>") 'windmove-left
          )
          (define-key
            eshell-mode-map
            (kbd "C-<right>") 'windmove-right
          )
          (define-key
            eshell-mode-map
            (kbd "C-l") 'eshell/clear
          )
          (define-key
            eshell-mode-map
            (kbd "C-u") 'kill-whole-line
          )
          (define-key
            eshell-mode-map
            (kbd "C-c k") (lambda ()
                        (interactive)
                        (pick-kubectx)
                        )
            )
          (define-key
            eshell-mode-map
            (kbd "C-c g") (lambda ()
                        (interactive)
                        (go-to-project)
                        )
            )
          (define-key
            eshell-mode-map
            (kbd "C-c w") (lambda ()
                         (interactive)
                         (pick-gcp-project)
                         )
            )
          )
        )

  (defun select-k8s-context (x)
    (shell-command (concat "kubectx " x))
    )

  (defun pick-kubectx ()
    "Select k8s context"
    (interactive)
    (setenv "KUBECTX_IGNORE_FZF" "y")
    (ivy-read "Select kubernetes context: " (split-string (shell-command-to-string "kubectx") "\n" t)
              :action '(1
                       ("o" select-k8s-context)
                       )
              )
    )

  (defun select-gcp-project (x)
    (shell-command (concat "gcloud config set project " x))
    )

  (defun pick-gcp-project ()
    "Select GCP proejct"
    (interactive)
    (ivy-read "Select GCP Project: " (split-string (shell-command-to-string "gcloud projects list | tail -n +2 | awk '{print $1}'") "\n" t)
              :action '(1
                        ("o" select-gcp-project)
                        )
              )
    )

  (defun go-to-project ()
    "Go to project"
    (interactive)
    (counsel-projectile-switch-project
     'counsel-projectile-switch-project-action-run-eshell)
    )

  (defun esh-prompt-func ()
    "Build `eshell-prompt-function'"
    (concat esh-header
            (-reduce-from 'esh-acc "" eshell-funcs)
            "\n"
            eshell-prompt-string))

  (esh-section esh-dir
               "\xf07c"  ;  (fontawesome folder)
               (abbreviate-file-name (eshell/pwd))
               '(:foreground "gold" :bold ultra-bold :underline t))

    (esh-section esh-git
               "\xf126"  ;  (git icon)
               (magit-get-current-branch)
               '(:foreground "pink"))

    (esh-section esh-git-changes
               "\xf071"  ;  (warn icon)
               (git-changes)
               '(:foreground "red"))

    (esh-section esh-git-unpushed-commits
               "\xf714"  ;  (skull icon)
               (git-unpushed-commits)
               '(:foreground "red"))

    (esh-section esh-k8s
               "\xf1b3 "  ;  (cubes icon)
               (k8s-context)
               '(:foreground "forest green"))

    (esh-section esh-gcp
               "\xf1a0"  ;  (google icon)
               (current-gcloud-project)
               '(:foreground "dark green"))

  ;; Separator between esh-sections
  (setq esh-sep "  ")

  ;; Separator between an esh-section icon and form
  (setq esh-section-delim " ")

  ;; Eshell prompt header
  (setq esh-header "\n")

  ;; Eshell prompt regexp and string. Unless you are varying the prompt by eg.
  ;; your login, these can be the same.
  (setq eshell-prompt-regexp "")
  (setq eshell-prompt-string "")

  ;; Choose which eshell-funcs to enable
  (setq eshell-funcs (list esh-dir esh-git
                           esh-git-changes
                           esh-git-unpushed-commits
                           esh-k8s esh-gcp))

  ;; Enable the new eshell prompt
  (setq eshell-prompt-function 'esh-prompt-func)

)

(require 'em-term)
(with-eval-after-load 'em-term
  (dolist (p '("fzf" "spook" "htop" "pinentry-curses"))
    (add-to-list 'eshell-visual-commands p)))
  (dolist (p '("git" ("log" "diff" "show") "home" ("log" "diff" "show"))
    (add-to-list 'eshell-visual-subcommands p)))
(add-hook 'eshell-mode-hook (
 lambda ()
        (setq show-trailing-whitespace nil)
        (setenv "PAGER" "cat")
        (setenv "EDITOR" "emacsclient")
        ))

(add-hook 'shell-mode-hook (
 lambda ()
        (setq show-trailing-whitespace nil)
        (setenv "PAGER" "cat")
        (setenv "EDITOR" "emacsclient")
        ))

(add-hook 'term-mode-hook (
 lambda ()
        (setq show-trailing-whitespace nil)
        (setenv "PAGER" "cat")
        (setenv "EDITOR" "emacsclient")
        ))

(setq temporary-file-directory "~/.emacs.d/tmp/")
(unless (file-exists-p "~/.emacs.d/tmp")
  (make-directory "~/.emacs.d/tmp"))

(setq backup-inhibited t)
(setq make-backup-files nil) ; don't create backup~ files
(setq auto-save-default nil) ; don't create #autosave# files


;; Helper for opening a new empty buffer.
(defun insane-new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

It returns the buffer (for elisp programing).

URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf
    ))

;; Finally, since I'm in Europe, I'd like dates and such to be displayed in the expected European formats.
(setq european-date-style 'european)
(setq calendar-set-date-style 'european)
(setq calendar-week-start-day 1)
(setq calendar-date-display-form
      '((if dayname
            (concat dayname ", "))
        day " " monthname " " year))

(setq calendar-time-display-form
      '(24-hours ":" minutes))

(customize-set-variable 'lsp-rust-server 'rust-analyzer)
(customize-set-variable 'nix-nixfmt-bin "nixpkgs-fmt")
