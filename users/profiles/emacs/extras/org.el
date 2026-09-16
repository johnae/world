;;; Emacs Bedrock
;;;
;;; Extra config: Org-mode starter config

;;; Usage: Append or require this file from init.el for some software
;;; development-focused packages.
;;;
;;; Org-mode is a fantastically powerful package. It does a lot of things, which
;;; makes it a little difficult to understand at first.
;;;
;;; We will configure Org-mode in phases. Work with each phase as you are
;;; comfortable.
;;;
;;; YOU NEED TO CONFIGURE SOME VARIABLES! The most important variable is the
;;; `org-directory', which tells org-mode where to look to find your agenda
;;; files.

;;; See "org-intro.txt" for a high-level overview.

;;; Contents:
;;;
;;;  - Critical variables
;;;  - Phase 1: editing and exporting files
;;;  - Phase 2: todos, agenda generation, and task tracking
;;;  - Phase 3: extensions (org-roam, etc.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Critical variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These variables need to be set for Org-mode's full power to be unlocked!
;;;
;;; You can read the documentation for any variable with `C-h v'. If you have
;;; Consult configured (see the `base.el' file) then it should help you find
;;; what you're looking for.

;;; Phase 1 variables

;;; Phase 2 variables

;; Agenda variables
(setq org-directory "~/Documents/org/") ; Non-absolute paths for agenda and
                                        ; capture templates will look here.

(setq org-agenda-files '("inbox.org" "work.org"))

;; Default tags
(setq org-tag-alist '(
                      ;; locale
                      (:startgroup)
                      ("home" . ?h)
                      ("work" . ?w)
                      ("school" . ?s)
                      (:endgroup)
                      (:newline)
                      ;; scale
                      (:startgroup)
                      ("one-shot" . ?o)
                      ("project" . ?j)
                      ("tiny" . ?t)
                      (:endgroup)
                      ;; misc
                      ("meta")
                      ("review")
                      ("reading")))

;; Org-refile: where should org-refile look?
(setq org-refile-targets 'FIXME)

;;; Phase 3 variables

;; Org-roam variables
(setq org-roam-directory "~/Development/org-roam/")
(setq org-roam-index-file "~/Development/org-roam/index.org")

;;; Optional variables

;; Advanced: Custom link types
;; This example is for linking a person's 7-character ID to their page on the
;; free genealogy website Family Search.
(setq org-link-abbrev-alist
      '(("family_search" . "https://www.familysearch.org/tree/person/details/%s")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Visual Enhancements for Terminal/TUI
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Org-superstar for beautiful bullets in terminal
(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :config
  ;; Terminal-friendly bullets that work well in TUI
  (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶"))
  (setq org-superstar-item-bullet-alist '((?* . 8902) (?+ . 8227) (?- . 8226)))
  ;; Hide leading stars but keep indentation
  (setq org-superstar-leading-bullet ?\s)
  (setq org-superstar-leading-fallback ?\s)
  ;; Make TODO keywords more visible
  (setq org-superstar-special-todo-items t))

;; Rainbow delimiters for code blocks
(use-package rainbow-delimiters
  :ensure t
  :hook ((org-mode . rainbow-delimiters-mode)
         (prog-mode . rainbow-delimiters-mode)))

;; Org-modern for additional visual improvements (terminal-compatible)
(use-package org-modern
  :ensure t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :config
  ;; Terminal-friendly settings
  (setq org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶"))
  (setq org-modern-list '((?* . 8902) (?+ . 8227) (?- . 8226)))
  (setq org-modern-checkbox '((88 . "☑") (45 . "☐") (32 . "☐")))
  (setq org-modern-table-vertical 1)
  (setq org-modern-table-horizontal 0.2)
  (setq org-modern-block-fringe nil)  ; Terminal doesn't have fringes
  ;; Tag styling that works in terminal
  (setq org-modern-tag t)
  (setq org-modern-priority t)
  ;; Timestamp styling
  (setq org-modern-timestamp t)
  (setq org-modern-statistics t)
  ;; Fold ellipsis
  (setq org-modern-fold-stars '(("▶" . "▼"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Phase 1: editing and exporting files
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :hook ((org-mode . visual-line-mode)  ; wrap lines at word breaks
         (org-mode . flyspell-mode)     ; spell checking!
         (org-mode . org-indent-mode)   ; Clean indentation
         (org-mode . (lambda ()
                       (setq-local line-spacing 0.1)  ; Slightly more line spacing
                       (variable-pitch-mode -1))))    ; Ensure monospace in terminal

  :bind (:map global-map
              ("C-c o s" . org-store-link)          ; Mnemonic: link → store
              ("C-c o i" . org-insert-link-global)  ; Mnemonic: link → insert
              ("C-c o a" . org-agenda)                 ; Quick agenda access
              ("C-c o c" . org-capture))               ; Quick capture
  :custom-face
  ;; Terminal-friendly face customizations
  (org-level-1 ((t (:height 1.2 :weight extra-bold :foreground "#51afef"))))
  (org-level-2 ((t (:height 1.15 :weight bold :foreground "#c678dd"))))
  (org-level-3 ((t (:height 1.1 :weight bold :foreground "#98be65"))))
  (org-level-4 ((t (:height 1.05 :weight semi-bold :foreground "#da8548"))))
  (org-level-5 ((t (:height 1.0 :weight semi-bold :foreground "#5699af"))))
  (org-level-6 ((t (:height 1.0 :weight semi-bold :foreground "#a9a1e1"))))
  (org-level-7 ((t (:height 1.0 :weight semi-bold :foreground "#46d9ff"))))
  (org-level-8 ((t (:height 1.0 :weight semi-bold :foreground "#ff6c6b"))))
  
  ;; Block delimiters and code blocks
  (org-block ((t (:background "#1e1e1e" :extend t))))
  (org-block-begin-line ((t (:foreground "#5c6370" :background "#1e1e1e" :extend t :slant italic))))
  (org-block-end-line ((t (:foreground "#5c6370" :background "#1e1e1e" :extend t :slant italic))))
  (org-code ((t (:foreground "#98be65" :background "#2e2e2e"))))
  (org-verbatim ((t (:foreground "#da8548" :background "#2e2e2e"))))
  
  ;; Special keywords
  (org-special-keyword ((t (:foreground "#5c6370" :slant italic))))
  (org-meta-line ((t (:foreground "#5c6370" :slant italic))))
  (org-drawer ((t (:foreground "#5c6370"))))
  (org-property-value ((t (:foreground "#a9a1e1"))))
  
  ;; Links
  (org-link ((t (:foreground "#51afef" :underline t))))
  (org-footnote ((t (:foreground "#c678dd" :underline t))))
  
  ;; Dates and timestamps
  (org-date ((t (:foreground "#a9a1e1" :underline nil))))
  (org-scheduled ((t (:foreground "#98be65"))))
  (org-scheduled-today ((t (:foreground "#98be65" :weight bold))))
  (org-scheduled-previously ((t (:foreground "#ff6c6b"))))
  (org-deadline-announce ((t (:foreground "#ff6c6b" :weight bold))))
  
  ;; Tables
  (org-table ((t (:foreground "#51afef"))))
  (org-table-header ((t (:foreground "#51afef" :weight bold :underline t))))
  
  ;; Tags
  (org-tag ((t (:foreground "#5699af" :weight normal :slant italic))))
  
  ;; Checkboxes
  (org-checkbox ((t (:foreground "#51afef" :background nil :box nil))))
  (org-checkbox-statistics-todo ((t (:foreground "#c678dd" :weight bold))))
  (org-checkbox-statistics-done ((t (:foreground "#98be65" :weight bold))))
  
  :config
  (require 'oc-csl)                     ; citation support
  (add-to-list 'org-export-backends 'md)

  ;; Make org-open-at-point follow file links in the same window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  ;; Make exporting quotes better
  (setq org-export-with-smart-quotes t)
  
  ;; Visual settings for better appearance
  (setq org-hide-emphasis-markers t)    ; Hide markup characters
  (setq org-pretty-entities t)          ; Display UTF-8 characters
  (setq org-ellipsis " ▼ ")             ; Nicer ellipsis for folded content
  (setq org-startup-indented t)         ; Start with indented view
  (setq org-hide-leading-stars t)       ; Hide leading stars
  (setq org-odd-levels-only nil)        ; Use all levels
  (setq org-adapt-indentation t)        ; Adapt indentation
  (setq org-src-fontify-natively t)     ; Syntax highlighting in code blocks
  (setq org-src-tab-acts-natively t)    ; Tab works natively in code blocks
  (setq org-edit-src-content-indentation 0) ; No extra indentation in src blocks
  (setq org-fontify-quote-and-verse-blocks t) ; Fontify quote blocks
  (setq org-fontify-whole-heading-line t) ; Extend background to full width
  
  ;; Better list bullets
  (setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))
  
  ;; Emphasis markers customization
  (setq org-emphasis-alist
        '(("*" (bold))
          ("/" (italic))
          ("_" (underline))
          ("=" org-verbatim verbatim)
          ("~" org-code verbatim)
          ("+" (:strike-through t))))
  
  ;; Priority faces with nice colors
  (setq org-priority-faces
        '((?A . (:foreground "#ff6c6b" :weight bold))
          (?B . (:foreground "#da8548" :weight semi-bold))
          (?C . (:foreground "#98be65" :weight normal))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Phase 2: todos, agenda generation, and task tracking
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Yes, you can have multiple use-package declarations. It's best if their
;; configs don't overlap. Once you've reached Phase 2, I'd recommend merging the
;; config from Phase 1. I've broken it up here for the sake of clarity.
(use-package org
  :config
  ;; Instead of just two states (TODO, DONE) we set up a few different states
  ;; that a task can be in.
  (setq org-todo-keywords
        '((sequence 
           "TODO(t)" 
           "NEXT(n)"
           "STARTED(s!)" 
           "WAITING(w@/!)" 
           "|" 
           "DONE(d!)" 
           "CANCELLED(c@)"
           "OBSOLETE(o@)")))
  
  ;; Beautiful TODO keyword faces with terminal-friendly colors
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "#ff6c6b" :weight bold))
          ("NEXT" . (:foreground "#da8548" :weight bold))
          ("STARTED" . (:foreground "#46d9ff" :weight bold))
          ("WAITING" . (:foreground "#ecbe7b" :weight bold))
          ("DONE" . (:foreground "#98be65" :weight bold))
          ("CANCELLED" . (:foreground "#5c6370" :weight bold :strike-through t))
          ("OBSOLETE" . (:foreground "#5c6370" :slant italic))))

  ;; Refile configuration
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)
  
  ;; Agenda visual improvements
  (setq org-agenda-breadcrumbs-separator " ❯ ")
  (setq org-agenda-current-time-string "◀── now ─────────────────────────────────────────────────")
  (setq org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  
  ;; Compact and clean agenda view
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t% s")
          (todo . " %i %-12:c")
          (tags . " %i %-12:c")
          (search . " %i %-12:c")))
  
  ;; Better agenda display
  (setq org-agenda-block-separator 9472)
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-tags-column -80)

  (setq org-capture-templates
        '(("c" "Default Capture" entry (file "inbox.org")
           "* TODO %?\n%U\n%i")
          ;; Capture and keep an org-link to the thing we're currently working with
          ("r" "Capture with Reference" entry (file "inbox.org")
           "* TODO %?\n%U\n%i\n%a")
          ;; Define a section
          ("w" "Work")
          ("wm" "Work meeting" entry (file+headline "work.org" "Meetings")
           "** TODO %?\n%U\n%i\n%a")
          ("wr" "Work report" entry (file+headline "work.org" "Reports")
           "** TODO %?\n%U\n%i\n%a")))

  (setq org-agenda-custom-commands
        '(("n" "Agenda and All Todos"
           ((agenda)
            (todo)))
          ("w" "Work" agenda ""
           ((org-agenda-files '("work.org")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Phase 3: extensions (org-roam, etc.)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-roam
  :ensure t
  :config
  (org-roam-db-autosync-mode)
  ;; Dedicated side window for backlinks
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.4)
                 (window-height . fit-window-to-buffer))))

;; Pretty web interface for org-roam
					;(use-package org-roam-ui
					;  :ensure t
					;  :after org-roam
					;  :config
					;  (setq org-roam-ui-sync-theme t
					;        org-roam-ui-follow t
					;        org-roam-ui-update-on-save t
					;        org-roam-ui-open-on-start t))
