;;; modeline.el --- Custom mode-line configuration -*- lexical-binding: t; -*-

;; A clean, minimal mode-line that works well in both TUI and GUI

(defgroup custom-modeline nil
  "Custom mode-line configuration."
  :group 'mode-line)

(defface custom-modeline-buffer-name
  '((t :inherit mode-line-buffer-id :weight bold))
  "Face for buffer name in mode-line."
  :group 'custom-modeline)

(defface custom-modeline-major-mode
  '((t :inherit mode-line-emphasis))
  "Face for major mode in mode-line."
  :group 'custom-modeline)

(defface custom-modeline-minor-mode
  '((t :inherit mode-line))
  "Face for minor modes in mode-line."
  :group 'custom-modeline)

(defface custom-modeline-info
  '((t :inherit success))
  "Face for info in mode-line."
  :group 'custom-modeline)

(defface custom-modeline-warning
  '((t :inherit warning))
  "Face for warnings in mode-line."
  :group 'custom-modeline)

(defface custom-modeline-error
  '((t :inherit error))
  "Face for errors in mode-line."
  :group 'custom-modeline)

(defface custom-modeline-modified
  '((t :inherit error :weight bold))
  "Face for modified status in mode-line."
  :group 'custom-modeline)

(defface custom-modeline-git-branch
  '((t :inherit success))
  "Face for git branch in mode-line."
  :group 'custom-modeline)

(defface custom-modeline-separator
  '((t :inherit shadow))
  "Face for separators in mode-line."
  :group 'custom-modeline)

;; Helper functions
(defun custom-modeline--separator ()
  "Return a separator string for mode-line."
  " │ ")

(defun custom-modeline--make-mouse-map (mouse function)
  "Return a mouse map for MOUSE and FUNCTION."
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'mode-line mouse) function)
    map))

;; Mode-line segments
(defvar-local custom-modeline-buffer-name
  '(:eval
    (propertize
     (if buffer-file-name
         (abbreviate-file-name buffer-file-name)
       (buffer-name))
     'face 'custom-modeline-buffer-name
     'help-echo (buffer-file-name)
     'mouse-face 'mode-line-highlight
     'local-map (custom-modeline--make-mouse-map 'mouse-1 'previous-buffer)))
  "Mode-line segment for buffer name.")

(defvar-local custom-modeline-modified
  '(:eval
    (when (and (buffer-modified-p) (buffer-file-name))
      (propertize " ✏️"
                  'face 'custom-modeline-modified
                  'help-echo "Buffer modified")))
  "Mode-line segment for modified status.")

(defvar-local custom-modeline-read-only
  '(:eval
    (when buffer-read-only
      (propertize " 🔒"
                  'face 'custom-modeline-warning
                  'help-echo "Buffer is read-only")))
  "Mode-line segment for read-only status.")

(defvar-local custom-modeline-position
  '(:eval
    (propertize
     (format " %d:%d "
             (line-number-at-pos)
             (current-column))
     'face 'mode-line
     'help-echo "Line:Column"))
  "Mode-line segment for cursor position.")

(defvar-local custom-modeline-region-info
  '(:eval
    (when (use-region-p)
      (propertize
       (format " [%d lines, %d chars] "
               (count-lines (region-beginning) (region-end))
               (- (region-end) (region-beginning)))
       'face 'custom-modeline-info
       'help-echo "Region info")))
  "Mode-line segment for region information.")

(defvar-local custom-modeline-major-mode
  '(:eval
    (propertize
     (format " %s " (format-mode-line mode-name))
     'face 'custom-modeline-major-mode
     'help-echo "Major mode\nmouse-1: Display major mode menu"
     'mouse-face 'mode-line-highlight
     'local-map (custom-modeline--make-mouse-map 'mouse-1 'mouse-major-mode-menu)))
  "Mode-line segment for major mode.")

;; VCS (jujutsu/git) support
(defun custom-modeline--jj-info ()
  "Get current jujutsu changeset info."
  (when (file-exists-p ".jj")
    (condition-case nil
        (let* ((cmd "jj log -r@ -n1 --ignore-working-copy --no-graph --color never -T 'separate(\" \", change_id.shortest(8), bookmarks, description.first_line().substr(0, 30))'")
               (output (string-trim (shell-command-to-string cmd)))
               (parts (split-string output " " t))
               (change-id (car parts))
               (rest (cdr parts)))
          (if rest
              (format "📦 %s %s" change-id (string-join rest " "))
            (format "📦 %s" change-id)))
      (error nil))))

(defun custom-modeline--git-branch ()
  "Get current git branch."
  (when (and (fboundp 'vc-git-mode-line-string)
             vc-mode
             (string-match "Git[:-]\\(.+\\)" vc-mode))
    (format "🌿 %s" (match-string 1 vc-mode))))

(defvar-local custom-modeline-vcs
  '(:eval
    (let ((vcs-info
           (cond
            ;; Check jujutsu first (preferred)
            ((custom-modeline--jj-info))
            ;; Fallback to git
            ((custom-modeline--git-branch))
            ;; No VCS
            (t nil))))
      (when vcs-info
        (propertize
         (format " %s" vcs-info)
         'face 'custom-modeline-git-branch
         'help-echo "Version control info"))))
  "Mode-line segment for VCS (jujutsu or git).")


(defvar-local custom-modeline-encoding
  '(:eval
    (unless (string= (symbol-name buffer-file-coding-system) "utf-8-unix")
      (propertize
       (format " %s " (symbol-name buffer-file-coding-system))
       'face 'custom-modeline-warning
       'help-echo "Buffer encoding")))
  "Mode-line segment for file encoding.")

(defvar-local custom-modeline-eol
  '(:eval
    (let ((eol (coding-system-eol-type buffer-file-coding-system)))
      (when (and (numberp eol) (> eol 0))
        (propertize
         (cond
          ((= eol 1) " CRLF ")
          ((= eol 2) " CR ")
          (t ""))
         'face 'custom-modeline-warning
         'help-echo "End-of-line style"))))
  "Mode-line segment for end-of-line style.")

(defvar-local custom-modeline-flycheck
  '(:eval
    (when (and (bound-and-true-p flycheck-mode)
               (bound-and-true-p flycheck-current-errors))
      (let* ((errors (flycheck-count-errors flycheck-current-errors))
             (err (or (cdr (assq 'error errors)) 0))
             (warn (or (cdr (assq 'warning errors)) 0))
             (info (or (cdr (assq 'info errors)) 0)))
        (propertize
         (format " ⚠️%d ❌%d ℹ️%d "
                 warn err info)
         'face (cond
                ((> err 0) 'custom-modeline-error)
                ((> warn 0) 'custom-modeline-warning)
                (t 'custom-modeline-info))))))
  "Mode-line segment for flycheck errors.")

(defvar-local custom-modeline-lsp
  '(:eval
    (when (bound-and-true-p lsp-mode)
      (propertize
       " 🔗 LSP "
       'face 'custom-modeline-info
       'help-echo "Language Server Protocol active")))
  "Mode-line segment for LSP status.")

;; Integration with meow/helix mode
(defvar-local custom-modeline-meow
  '(:eval
    (cond
     ((bound-and-true-p meow-insert-mode)
      (propertize " 📝 INSERT " 'face 'custom-modeline-info))
     ((bound-and-true-p meow-normal-mode)
      (propertize " 🎯 NORMAL " 'face 'custom-modeline-major-mode))
     ((bound-and-true-p meow-motion-mode)
      (propertize " 🏃 MOTION " 'face 'custom-modeline-warning))
     ((bound-and-true-p meow-keypad-mode)
      (propertize " ⌨️ KEYPAD " 'face 'custom-modeline-error))
     ((and (fboundp 'meow-helix-modeline-indicator)
           (not (string= (meow-helix-modeline-indicator) "")))
      (propertize (concat " 👁️" (meow-helix-modeline-indicator) " ") 'face 'custom-modeline-info))
     (t "")))
  "Mode-line segment for meow mode.")

(defvar-local custom-modeline-misc-info
  '(:eval
    (let ((misc (format-mode-line mode-line-misc-info)))
      (unless (string= misc "")
        (propertize misc 'face 'mode-line))))
  "Mode-line segment for misc info.")

;; Construct the mode-line format
(defun custom-modeline-setup ()
  "Setup the custom mode-line."
  (setq-default mode-line-format
                '((:eval
                   (let* ((left (list
                                 " "
                                 custom-modeline-meow
                                 custom-modeline-modified
                                 custom-modeline-read-only
                                 " "
                                 custom-modeline-buffer-name
                                 custom-modeline-position
                                 custom-modeline-region-info))
                          (center (list
                                   custom-modeline-major-mode
                                   custom-modeline-vcs))
                          (right (list
                                  custom-modeline-flycheck
                                  custom-modeline-lsp
                                  custom-modeline-encoding
                                  custom-modeline-eol
                                  custom-modeline-misc-info
                                  " "))
                          (available-width (- (window-width)
                                              (length (format-mode-line left))
                                              (length (format-mode-line right))))
                          (center-width (length (format-mode-line center)))
                          (center-padding (/ (- available-width center-width) 2)))
                     (append
                      left
                      (when (> center-padding 0)
                        (list (make-string center-padding ?\s)))
                      center
                      (when (> center-padding 0)
                        (list (make-string center-padding ?\s)))
                      right))))))

;; Additional customizations
(defun custom-modeline-adaptive-colors ()
  "Adapt mode-line colors based on terminal capabilities."
  (when (not (display-graphic-p))
    ;; Simplify faces for terminal
    (set-face-attribute 'mode-line nil
                        :box nil
                        :underline nil
                        :overline nil)
    (set-face-attribute 'mode-line-inactive nil
                        :box nil
                        :underline nil
                        :overline nil)))

;; Clean up default mode-line clutter
(defun custom-modeline-cleanup ()
  "Remove unnecessary elements from mode-line."
  ;; Hide minor modes we don't need to see
  (setq mode-line-modes
        (let ((recursive-edit-help-echo "Recursive edit, type C-M-c to get out"))
          (list (propertize "%[" 'help-echo recursive-edit-help-echo)
                "("
                `(:propertize ("" mode-name)
                              help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
                              mouse-face mode-line-highlight
                              local-map ,mode-line-major-mode-keymap)
                '("" mode-line-process)
                ")"
                (propertize "%]" 'help-echo recursive-edit-help-echo)))))

;; Initialize
(custom-modeline-setup)
(custom-modeline-adaptive-colors)
(custom-modeline-cleanup)

;; Hook for terminal/GUI changes
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (custom-modeline-adaptive-colors))))

(provide 'modeline)
;;; modeline.el ends here