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

;; Initialize [[https://github.com/jwiegley/use-package][use-package]].
(eval-and-compile
  (require 'package)
  (package-initialize)
  (require 'use-package))

;; Add path to mu4e to load-path.
(add-to-list 'load-path "@MUSE_LOAD_PATH@")

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
         '(("a" "My TODO task format." entry
          (file "~/Sync/org/todos.org")
           "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")))
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

;; The awesome Mu4e email client. (This is added to the load path as it comes with mu).

(setq uninteresting-mail-query
      (concat
       "from:/\\(hello|kooperativa|usafis|stadiummember|info-sas|.*no.?reply|store-news|newblack|stockholm\.soder|newsletter|.*campaign.*\\)@/"
       " OR from:notifications@github.com"
       " OR flag:trashed"
       " OR flag:list"
       " OR maildir:/Trash/"
       " OR maildir:/Commercial/"
       " OR maildir:/Junk/"
       " OR maildir:/All.Mail/"))

(require 'mu4e)
(setq mail-user-agent 'mu4e-user-agent
      mu4e-maildir "~/.mail"
      ;;mu4e-compose-format-flowed t
      mu4e-sent-messages-behavior (lambda ()
        (if (not(string= (message-sendmail-envelope-from) "john@insane.se"))
            'delete
          'sent))
      mu4e-headers-date-format "%Y-%m-%d"
      mu4e-headers-time-format "%H:%M"
      mu4e-headers-skip-duplicates t
      mu4e-compose-dont-reply-to-self t
      mu4e-compose-crypto-reply-policy 'sign-and-encrypt
      mu4e-enable-async-operations t
      mu4e-view-prefer-html nil
      mu4e-hide-index-messages t
      mu4e-change-filenames-when-moving t
      mu4e-split-view 'horizontal
      mu4e-view-show-addresses t
      org-mu4e-convert-to-html t
      mu4e-headers-leave-behavior 'apply
      mu4e-headers-include-related t

      mu4e-use-fancy-chars t
      mu4e-headers-unread-mark    '("u" . " ")
      mu4e-headers-new-mark       '("N" . " ")
      mu4e-headers-draft-mark     '("D" . "⚒ ")
      mu4e-headers-passed-mark    '("P" . "❯ ")
      mu4e-headers-replied-mark   '("R" . "❮ ")
      mu4e-headers-seen-mark      '("S" . "✔ ")
      mu4e-headers-attach-mark    '("" . "⚓")
      mu4e-headers-flagged-mark   '("F" . "✚ ")
      mu4e-headers-trashed-mark   '("T" . " ")
      mu4e-headers-encrypted-mark '("x" . "  ")
      mu4e-headers-signed-mark    '("s" . " ")

      ;;mu4e-html2text-command 'mu4e-shr2text
      mu4e-html2text-command "iconv -c -t utf-8 | @PANDOC@ -f html -t plain"
      ;;mu4e-html2text-command "w3m -dump -T text/html -cols 72 -o display_link_number=true -o auto_image=false -o display_image=false -o ignore_null_img_alt=true"
      mu4e-get-mail-command "@MBSYNC@ -a"
      mu4e-update-interval 1200 ;; we are using imapnotify so not super important
      mu4e-view-fields '(:from :to :cc :subject :flags :date :maildir :mailing-list :tags :attachments :signature :decryption))

(add-to-list 'mu4e-view-actions
    '("ViewInBrowser" . mu4e-action-view-in-browser) t)

(add-hook 'mu4e-mark-execute-pre-hook
    (lambda (mark msg)
      (cond ((member mark '(refile trash)) (mu4e-action-retag-message msg "-\\Inbox"))
      ((equal mark 'flag) (mu4e-action-retag-message msg "\\Starred"))
      ((equal mark 'unflag) (mu4e-action-retag-message msg "-\\Starred")))))

(add-hook 'mu4e-view-mode-hook
          (lambda ()
            (setq mu4e-view-show-images t)))

(add-hook 'mu4e-compose-mode-hook
          (lambda ()
            (set-fill-column 72)
            (auto-fill-mode 0)
            (visual-fill-column-mode)
            (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
            (visual-line-mode)))

(require 'org-mu4e)
(setq org-mu4e-link-query-in-headers-mode nil)

;; Mu4e alert. For notifications on new mail.

;; (use-package mu4e-alert
;;     :after mu4e
;;     :init
;;     :config
;;     (setq mu4e-alert-icon "mail-generic")
;;     (mu4e-alert-set-default-style 'libnotify)
;;     ;;(alert-add-rule :category "mu4e-alert" :icon "mail-generic" :continue t)
;;     (setq mu4e-alert-email-notification-types '(subjects))
;;     (setq mu4e-alert-interesting-mail-query
;;       (concat
;;        "date:today..now"
;;        " AND flag:unread"
;;        " AND NOT (" uninteresting-mail-query ") "))
;;     (mu4e-alert-enable-mode-line-display)
;;     (mu4e-alert-enable-notifications)
;; )


;; Visual-fill-column. Helps with composing emails.
(use-package visual-fill-column
  :ensure t)

;; Sending email.
(require 'jl-encrypt)
(setq mml-secure-insert-signature "always")

(setq mu4e-bookmarks
  `( ,(make-mu4e-bookmark
       :name  "Unread messages"
       :query (concat "flag:unread"
                      " AND NOT flag:trashed"
                      " AND NOT maildir:/All.Mail/"
                      " AND NOT maildir:/Junk/"
                      " AND NOT maildir:/Commercial/"
                      " AND NOT flag:list")
       :key ?u)
     ,(make-mu4e-bookmark
       :name "Today's messages"
       :query (concat "date:today..now"
                      " AND maildir:/inbox/"
                      " AND NOT (" uninteresting-mail-query ")")
       :key ?t)
     ,(make-mu4e-bookmark
       :name "Today's lists"
       :query (concat "date:today..now"
                      " AND flag:list")
       :key ?m)
     ,(make-mu4e-bookmark
       :name "Last 7 days"
       :query (concat "date:7d..now"
                      " AND maildir:/inbox/"
                      " AND NOT (" uninteresting-mail-query ")")
       :key ?w)
     ,(make-mu4e-bookmark
       :name "Last 7 days of lists"
       :query (concat "date:7d..now"
                      " AND flag:list")
       :key ?l)
     ,(make-mu4e-bookmark
       :name "Flagged in INBOX"
       :query (concat "flag:flagged"
                      " AND maildir:/inbox/")
       :key ?f))
)

(setq user-mail-address "john@insane.se"
      mu4e-user-mail-address-list '("john@insane.se" "john@.karma.life" "john@karma.ly" "john@instabox.se")
      message-kill-buffers-on-exit t
      user-full-name "John Axel Eriksson"
      send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-user "john@insane.se"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      mu4e-sent-folder "/personal/Sent"
      mu4e-drafts-folder "/personal/Drafts"
      mu4e-trash-folder "/personal/Trash"
      mu4e-refile-folder "/personal/Archive"
)

(defvar my-mu4e-account-alist
  '(("personal"
     ;:(mu4e-sent-folder "/Gmail/sent")
     (user-mail-address "john@insane.se")
     (smtpmail-smtp-user "john@insane.se")
     (smtpmail-local-domain "insane.se")
     (smtpmail-default-smtp-server "smtp.gmail.com")
     (smtpmail-smtp-server "smtp.gmail.com")
     (smtpmail-smtp-service 587)
     (mu4e-sent-folder "/personal/[Gmail]/Sent Mail")
     (mu4e-drafts-folder "/personal/[Gmail]/Drafts")
     (mu4e-trash-folder "/personal/[Gmail]/Trash")
     (mu4e-refile-folder "/personal/[Gmail]/All Mail")
     )
    ("professional"
     ;;(mu4e-sent-folder "/Gmail/sent")
     (user-mail-address "john@instabox.se")
     (smtpmail-smtp-user "john@instabox.se")
     (smtpmail-local-domain "instabox.se")
     (smtpmail-default-smtp-server "smtp.gmail.com")
     (smtpmail-smtp-server "smtp.gmail.com")
     (smtpmail-smtp-service 587)
     (mu4e-sent-folder "/professional/[Gmail]/Sent Mail")
     (mu4e-drafts-folder "/professional/[Gmail]/Drafts")
     (mu4e-trash-folder "/professional/[Gmail]/Trash")
     (mu4e-refile-folder "/professional/[Gmail]/All Mail")
    )
    ("work"
     ;;(mu4e-sent-folder "/Gmail/sent")
     (user-mail-address "john@karma.life")
     (smtpmail-smtp-user "john@karma.life")
     (smtpmail-local-domain "karma.life")
     (smtpmail-default-smtp-server "smtp.gmail.com")
     (smtpmail-smtp-server "smtp.gmail.com")
     (smtpmail-smtp-service 587)
     (mu4e-sent-folder "/work/[Gmail]/Sent Mail")
     (mu4e-drafts-folder "/work/[Gmail]/Drafts")
     (mu4e-trash-folder "/work/[Gmail]/Trash")
     (mu4e-refile-folder "/work/[Gmail]/All Mail")
    ))
)

(defun my-mu4e-set-account ()
  "Set the account for composing a message.
   This function is taken from:
     https://www.djcbsoftware.nl/code/mu/mu4e/Multiple-accounts.html"
  (let* ((account
    (if mu4e-compose-parent-message
        (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
    (string-match "/\\(.*?\\)/" maildir)
    (match-string 1 maildir))
      (completing-read (format "Compose with account: (%s) "
             (mapconcat #'(lambda (var) (car var))
            my-mu4e-account-alist "/"))
           (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
           nil t nil nil (caar my-mu4e-account-alist))))
   (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
  (mapc #'(lambda (var)
      (set (car var) (cadr var)))
        account-vars)
      (error "No email account found"))))
(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

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

;; Manage external processes from within emacs.
;; (use-package prodigy
;;   :ensure t
;;   :init
;;   (prodigy-define-tag
;;    :name 'email
;;    :ready-message "Watching for new email using imap idle. Ctrl-C to shutdown.")
;;   (prodigy-define-service
;;     :name "imapnotify-karma"
;;     :command "@IMAPNOTIFY@"
;;     :args (list "-c" (expand-file-name ".config/imapnotify.work.js" (getenv "HOME")))
;;     :tags '(email)
;;     :kill-signal 'sigkill)
;;   (prodigy-define-service
;;     :name "imapnotify-insane"
;;     :command "@IMAPNOTIFY@"
;;     :args (list "-c" (expand-file-name ".config/imapnotify.insane-gmail.js" (getenv "HOME")))
;;     :tags '(email)
;;     :kill-signal 'sigkill)
;;   (prodigy-start-service (prodigy-find-service "imapnotify-karma"))
;;   (prodigy-start-service (prodigy-find-service "imapnotify-insane"))
;; )

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