(defun my/meow-save ()
  (interactive)
  (save-excursion (meow-save)))

(defun meow-helix-copy ()
  "Helix-style copy that preserves the original selection."
  (interactive)
  (when (region-active-p)
    (let ((start (region-beginning))
          (end (region-end)))
      (kill-ring-save start end)
      ;; Keep the original selection active (don't move point or mark)
      (setq deactivate-mark nil))))

(defun meow-helix-paste-after ()
  "Helix-style paste that places text after selection with proper indentation."
  (interactive)
  (if (region-active-p)
      (let* ((start (region-beginning))
             (end (region-end))
             (text (current-kill 0))
             (col (save-excursion
                    (goto-char start)
                    (current-column))))
        ;; Move to end of selection
        (goto-char end)
        ;; If we're mid-line, go to next line
        (unless (bolp)
          (end-of-line)
          (newline)
          (indent-to col))
        ;; Insert the text and select it
        (let ((insert-start (point)))
          (insert text)
          (let ((insert-end (point)))
            ;; Select the newly pasted text
            (goto-char insert-start)
            (set-mark insert-end)
            (setq deactivate-mark nil))))
    ;; If no selection, paste on next line
    (end-of-line)
    (newline)
    (let ((paste-start (point)))
      (yank)
      ;; Select the pasted text
      (set-mark paste-start)
      (setq deactivate-mark nil))))

(defun meow-helix-paste-before ()
  "Helix-style paste that places text before selection."
  (interactive)
  (if (region-active-p)
      (let* ((start (region-beginning))
             (end (region-end))
             (text (current-kill 0))
             (col (save-excursion
                    (goto-char start)
                    (current-column))))
        ;; Move to start of selection
        (goto-char start)
        ;; Go to beginning of line and insert
        (if (bolp)
            (progn
              (open-line 1)
              (indent-to col))
          (progn
            (beginning-of-line)
            (open-line 1)
            (indent-to col)))
        ;; Insert the text and select it
        (let ((insert-start (point)))
          (insert text)
          (let ((insert-end (point)))
            ;; Select the newly pasted text
            (goto-char insert-start)
            (set-mark insert-end)
            (setq deactivate-mark nil))))
    ;; If no selection, paste on previous line
    (beginning-of-line)
    (open-line 1)
    (let ((paste-start (point)))
      (yank)
      ;; Select the pasted text
      (set-mark paste-start)
      (setq deactivate-mark nil))))

(defun meow-helix-delete ()
  "Delete selection without extending, like Helix's d command."
  (interactive)
  (if (region-active-p)
      (delete-region (region-beginning) (region-end))
    ;; If no selection, do nothing (or select current char first)
    (progn
      (set-mark (point))
      (forward-char 1)
      (delete-region (region-beginning) (region-end)))))

(defun meow-helix-toggle-comment ()
  "Toggle comment on current line or region, like Helix's C-c."
  (interactive)
  (if (region-active-p)
      ;; Comment/uncomment region
      (comment-or-uncomment-region (region-beginning) (region-end))
    ;; Comment/uncomment current line
    (save-excursion
      (beginning-of-line)
      (set-mark (point))
      (end-of-line)
      (comment-or-uncomment-region (region-beginning) (region-end)))))

(defvar meow-helix-ex-commands
  '(("w" . save-buffer)
    ("write" . save-buffer)
    ("q" . kill-current-buffer)
    ("quit" . kill-current-buffer)
    ("wq" . (lambda () (interactive) (save-buffer) (kill-current-buffer)))
    ("x" . (lambda () (interactive) (save-buffer) (kill-current-buffer)))
    ("qa" . save-buffers-kill-emacs)
    ("qa!" . kill-emacs)
    ("e" . find-file)
    ("edit" . find-file)
    ("b" . switch-to-buffer)
    ("buffer" . switch-to-buffer)
    ("bn" . next-buffer)
    ("bp" . previous-buffer)
    ("bd" . kill-current-buffer)
    ("sp" . split-window-vertically)
    ("split" . split-window-vertically)
    ("vs" . split-window-horizontally)
    ("vsplit" . split-window-horizontally)
    ("make" . compile)
    ("!" . shell-command)
    ("term" . vterm)
    ("git" . magit-status)
    ("reload" . (lambda () (interactive) (revert-buffer t t)))
    ("config-reload" . (lambda () (interactive) (load-file user-init-file)))
    ("theme" . load-theme))
  "Helix-style ex commands mapping.")

(defun meow-helix-ex-command ()
  "Helix-style command mode, like typing : in Helix."
  (interactive)
  (let* ((input (read-string ": "))
         (parts (split-string input " "))
         (cmd (car parts))
         (args (cdr parts))
         (command (cdr (assoc cmd meow-helix-ex-commands))))
    (cond
     ;; Known ex command
     (command
      (if args
          ;; If command has arguments, call with arguments
          (if (commandp command)
              (call-interactively command)
            (funcall command))
        ;; No arguments, just call the command
        (if (commandp command)
            (call-interactively command)
          (funcall command))))
     ;; Check if it's a line number (like :42 to go to line 42)
     ((string-match "^[0-9]+$" cmd)
      (goto-line (string-to-number cmd)))
     ;; Fall back to execute-extended-command for any Emacs command
     (t
      (let ((command-sym (intern input)))
        (if (commandp command-sym)
            (call-interactively command-sym)
          ;; If not a command, try M-x style
          (execute-extended-command nil input)))))))

(defun paste-below ()
  "Paste copied text below."
  (interactive)
  (next-line)
  (meow-yank)
  (previous-line))

(defun meow-helix-change-inner ()
  "Helix-style change: select and delete, then enter insert."
  (interactive)
  (meow-inner-of-thing)
  (meow-kill)
  (meow-insert))

(defun meow-helix-change-around ()
  "Helix-style change around: select around and delete, then enter insert."
  (interactive)
  (meow-bounds-of-thing)
  (meow-kill)
  (meow-insert))

(defun meow-helix-select-line ()
  "Helix-style line selection."
  (interactive)
  (beginning-of-line)
  (set-mark (point))
  (end-of-line))

(defun meow-helix-extend-line ()
  "Extend selection to full lines."
  (interactive)
  (when (region-active-p)
    (let ((start (region-beginning))
          (end (region-end)))
      (goto-char start)
      (beginning-of-line)
      (set-mark (point))
      (goto-char end)
      (end-of-line))))

(defun meow-helix-goto-file-start ()
  "Go to file start (gg in Helix)."
  (interactive)
  (goto-char (point-min)))

(defun meow-helix-goto-file-end ()
  "Go to file end (ge in Helix)."
  (interactive)
  (goto-char (point-max)))

(defun meow-helix-goto-line-start ()
  "Go to first non-whitespace character of line."
  (interactive)
  (beginning-of-line)
  (skip-chars-forward " \t"))

(defun meow-helix-goto-line-end ()
  "Go to end of line."
  (interactive)
  (end-of-line))

(defun meow-insert-at-line-start ()
  "Move to line start and enter insert mode (Helix I)."
  (interactive)
  (beginning-of-line)
  (skip-chars-forward " \t")
  (meow-insert))

(defun meow-append-at-line-end ()
  "Move to line end and enter insert mode (Helix A)."
  (interactive)
  (end-of-line)
  (meow-insert))

(defun meow-helix-select-word ()
  "Select word under cursor (Helix w)."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (when bounds
      (goto-char (car bounds))
      (set-mark (cdr bounds)))))

(defun meow-helix-extend-word ()
  "Extend selection by word (Helix W)."
  (interactive)
  (if (region-active-p)
      (progn
        (goto-char (region-end))
        (forward-word))
    (meow-helix-select-word)))

(defun meow-helix-find-char ()
  "Find character (Helix f)."
  (interactive)
  (let ((char (read-char "Find char: ")))
    (forward-char)
    (search-forward (char-to-string char) nil t)
    (backward-char)))

(defun meow-helix-find-till ()
  "Find till character (Helix t)."
  (interactive)
  (let ((char (read-char "Till char: ")))
    (forward-char)
    (search-forward (char-to-string char) nil t)
    (backward-char 2)))

(defun meow-helix-match-brackets ()
  "Jump to matching bracket (Helix m)."
  (interactive)
  (cond
   ((looking-at "[({[]") (forward-sexp))
   ((looking-back "[)}\\]]" 1) (backward-sexp))
   (t (message "Not on a bracket"))))

(defun meow-helix-select-all ()
  "Select all text in buffer (Helix Ctrl-a)."
  (interactive)
  (mark-whole-buffer))

(defun meow-helix-select-whole-file ()
  "Select whole file like Helix % command."
  (interactive)
  (goto-char (point-min))
  (set-mark (point))
  (goto-char (point-max))
  (setq deactivate-mark nil))

(defun meow-helix-join-lines ()
  "Join lines (Helix J)."
  (interactive)
  (if (region-active-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (goto-char start)
        (while (< (point) end)
          (end-of-line)
          (delete-char 1)
          (just-one-space)))
    (join-line 1)))

(defun meow-helix-duplicate ()
  "Duplicate current line or selection (Helix C)."
  (interactive)
  (if (region-active-p)
      (let ((text (buffer-substring (region-beginning) (region-end))))
        (goto-char (region-end))
        (insert text))
    (let ((line (thing-at-point 'line t)))
      (end-of-line)
      (newline)
      (insert (string-trim-right line)))))

(defun meow-helix-indent ()
  "Indent selection or current line (Helix >)."
  (interactive)
  (if (region-active-p)
      (indent-rigidly (region-beginning) (region-end) 2)
    (indent-rigidly (line-beginning-position) (line-end-position) 2)))

(defun meow-helix-unindent ()
  "Unindent selection or current line (Helix <)."
  (interactive)
  (if (region-active-p)
      (indent-rigidly (region-beginning) (region-end) -2)
    (indent-rigidly (line-beginning-position) (line-end-position) -2)))

(defun meow-helix-page-up ()
  "Page up (Helix Ctrl-b)."
  (interactive)
  (scroll-down-command))

(defun meow-helix-page-down ()
  "Page down (Helix Ctrl-f)."
  (interactive)
  (scroll-up-command))

(defun meow-helix-half-page-up ()
  "Half page up (Helix Ctrl-u)."
  (interactive)
  (scroll-down (/ (window-height) 2)))

(defun meow-helix-half-page-down ()
  "Half page down (Helix Ctrl-d)."
  (interactive)
  (scroll-up (/ (window-height) 2)))

(defun meow-helix-search-next ()
  "Search next occurrence (Helix n)."
  (interactive)
  (isearch-repeat-forward))

(defun meow-helix-search-prev ()
  "Search previous occurrence (Helix N)."
  (interactive)
  (isearch-repeat-backward))

(defun meow-helix-visual-goto-line-start ()
  "Go to line start and extend selection in visual mode."
  (interactive)
  (if meow-helix-visual-mode
      (progn
        (unless (region-active-p)
          (set-mark (point)))
        (meow-helix-goto-line-start)
        (setq deactivate-mark nil))
    (meow-helix-goto-line-start)))

(defun meow-helix-visual-goto-line-end ()
  "Go to line end and extend selection in visual mode."
  (interactive)
  (if meow-helix-visual-mode
      (progn
        (unless (region-active-p)
          (set-mark (point)))
        (meow-helix-goto-line-end)
        (setq deactivate-mark nil))
    (meow-helix-goto-line-end)))

(defvar meow-helix-goto-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'meow-helix-visual-goto-file-start)
    (define-key map "e" 'meow-helix-visual-goto-file-end)
    ;; Removed h and l bindings - they conflict with normal movement
    ;; In Helix, gh and gl are accessed via g prefix, not standalone h/l
    (define-key map "s" 'meow-helix-visual-goto-line-start)
    (define-key map "d" 'xref-find-definitions)
    (define-key map "r" 'xref-find-references)
    (define-key map "i" 'imenu)
    (define-key map "t" 'beginning-of-defun)
    (define-key map "b" 'end-of-defun)
    (define-key map "f" 'project-find-file)
    (define-key map "." 'goto-last-change)
    map)
  "Keymap for Helix-style goto mode.")

(defun meow-helix-goto-mode ()
  "Enter Helix-style goto mode (g prefix)."
  (interactive)
  (set-transient-map meow-helix-goto-mode-map))


(defvar meow-helix-space-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "f" 'project-find-file)
    (define-key map "F" 'find-file)
    (define-key map "b" 'switch-to-buffer)
    (define-key map "B" 'ibuffer)
    (define-key map "s" 'save-buffer)
    (define-key map "S" 'save-some-buffers)
    (define-key map "c" 'meow-helix-toggle-comment)  ;; Comment toggle
    (define-key map "q" 'save-buffers-kill-emacs)
    (define-key map "Q" 'kill-emacs)
    (define-key map "w" 'meow-helix-window-mode)  ;; Changed to enter window mode
    (define-key map "W" 'delete-other-windows)
    (define-key map "v" 'split-window-right)
    (define-key map "h" 'split-window-below)
    (define-key map "o" 'other-window)
    (define-key map "/" 'meow-helix-project-search)  ;; Project-wide search
    (define-key map "k" 'kill-current-buffer)
    (define-key map "K" 'kill-buffer)
    (define-key map "g" 'magit-status)
    (define-key map "p" 'project-switch-project)
    (define-key map "e" 'eval-expression)
    (define-key map ":" 'execute-extended-command)
    (define-key map "!" 'shell-command)
    (define-key map "&" 'async-shell-command)
    map)
  "Keymap for Helix-style space mode.")

(defun meow-helix-space-mode ()
  "Enter Helix-style space mode (space leader key)."
  (interactive)
  (set-transient-map meow-helix-space-mode-map))

;; Project-wide search and replace functions
(defun meow-helix-project-search ()
  "Search across the project using deadgrep.
Press C-c C-p to enter edit mode, C-c C-c to save changes."
  (interactive)
  (if (fboundp 'deadgrep)
      (call-interactively 'deadgrep)
    ;; Fallback to consult-ripgrep if available
    (if (fboundp 'consult-ripgrep)
        (progn
          (consult-ripgrep)
          (message "Press C-c e to export, then C-c C-p to edit"))
      (call-interactively 'project-find-regexp))))


(defun meow-helix-search-word-at-point ()
  "Search for the word at point across the project."
  (interactive)
  (let ((word (thing-at-point 'symbol t)))
    (if word
        (if (fboundp 'deadgrep)
            (deadgrep word)
          (if (fboundp 'consult-ripgrep)
              (consult-ripgrep nil word)
            (project-find-regexp word)))
      (message "No word at point"))))

(defun meow-helix-search-symbol-at-point ()
  "Search for symbol at point in current buffer.
With prefix arg, search across the project."
  (interactive)
  (if current-prefix-arg
      (meow-helix-search-word-at-point)
    (isearch-forward-symbol-at-point)))

(defvar meow-helix-window-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "w" 'other-window)
    (define-key map "h" 'windmove-left)
    (define-key map "j" 'windmove-down)
    (define-key map "k" 'windmove-up)
    (define-key map "l" 'windmove-right)
    (define-key map (kbd "<left>") 'windmove-left)
    (define-key map (kbd "<down>") 'windmove-down)
    (define-key map (kbd "<up>") 'windmove-up)
    (define-key map (kbd "<right>") 'windmove-right)
    (define-key map "H" 'shrink-window-horizontally)
    (define-key map "J" 'enlarge-window)
    (define-key map "K" 'shrink-window)
    (define-key map "L" 'enlarge-window-horizontally)
    (define-key map "q" 'delete-window)
    (define-key map "Q" 'kill-buffer-and-window)
    (define-key map "o" 'delete-other-windows)
    (define-key map "s" 'split-window-below)
    (define-key map "v" 'split-window-right)
    map)
  "Keymap for Helix-style window mode.")

(defun meow-helix-window-mode ()
  "Enter Helix-style window mode (C-w prefix)."
  (interactive)
  (set-transient-map meow-helix-window-mode-map))

;;; Visual/Select Mode (Helix-style extend mode)
(defvar meow-helix-visual-mode nil
  "Whether visual mode is active (movements extend selection).")
(make-variable-buffer-local 'meow-helix-visual-mode)

(defvar meow-helix-visual-mode-cursor-color "#ff79c6"
  "Cursor color to use in visual mode.")

(defvar meow-helix-original-cursor-color nil
  "Store original cursor color before entering visual mode.")

(defun meow-helix-visual-mode-on ()
  "Enable visual mode where all movements extend selection."
  (setq meow-helix-visual-mode t)
  ;; Store and change cursor color
  (setq meow-helix-original-cursor-color (face-background 'cursor))
  (set-cursor-color meow-helix-visual-mode-cursor-color)
  ;; Start selection if none exists
  (unless (region-active-p)
    (set-mark (point))
    (setq deactivate-mark nil))
  (message "-- VISUAL --"))

(defun meow-helix-visual-mode-off ()
  "Disable visual mode."
  (when meow-helix-visual-mode
    (setq meow-helix-visual-mode nil)
    ;; Restore cursor color
    (when meow-helix-original-cursor-color
      (set-cursor-color meow-helix-original-cursor-color))
    (message "Normal mode")))

(defun meow-helix-toggle-visual-mode ()
  "Toggle visual selection mode where all movements extend."
  (interactive)
  (if meow-helix-visual-mode
      (meow-helix-visual-mode-off)
    (meow-helix-visual-mode-on)))

;; Visual mode movement commands that extend selection
(defun meow-helix-visual-next ()
  "Move down and extend selection in visual mode."
  (interactive)
  (if meow-helix-visual-mode
      (progn
        (unless (region-active-p)
          (set-mark (point)))
        (forward-line 1)
        (setq deactivate-mark nil))
    (call-interactively 'meow-next)))

(defun meow-helix-visual-prev ()
  "Move up and extend selection in visual mode."
  (interactive)
  (if meow-helix-visual-mode
      (progn
        (unless (region-active-p)
          (set-mark (point)))
        (forward-line -1)
        (setq deactivate-mark nil))
    (call-interactively 'meow-prev)))

(defun meow-helix-left ()
  "Move left, handling visual mode properly."
  (interactive)
  (if meow-helix-visual-mode
      (progn
        (unless (region-active-p)
          (set-mark (point)))
        (backward-char 1)
        (setq deactivate-mark nil))
    ;; Normal mode - just use backward-char directly
    (backward-char 1)))

(defun meow-helix-right ()
  "Move right, handling visual mode properly."
  (interactive)
  (if meow-helix-visual-mode
      (progn
        (unless (region-active-p)
          (set-mark (point)))
        (forward-char 1)
        (setq deactivate-mark nil))
    ;; Normal mode - just use forward-char directly
    (forward-char 1)))

(defun meow-helix-visual-left ()
  "Deprecated - use meow-helix-left instead."
  (interactive)
  (meow-helix-left))

(defun meow-helix-visual-right ()
  "Deprecated - use meow-helix-right instead."
  (interactive)
  (meow-helix-right))

(defun meow-helix-visual-next-word ()
  "Move to next word and extend selection in visual mode."
  (interactive)
  (if meow-helix-visual-mode
      (progn
        (unless (region-active-p)
          (set-mark (point)))
        (forward-word 1)
        (setq deactivate-mark nil))
    (call-interactively 'meow-next-word)))

(defun meow-helix-visual-back-word ()
  "Move to previous word and extend selection in visual mode."
  (interactive)
  (if meow-helix-visual-mode
      (progn
        (unless (region-active-p)
          (set-mark (point)))
        (backward-word 1)
        (setq deactivate-mark nil))
    (call-interactively 'meow-back-word)))

(defun meow-helix-visual-line-start ()
  "Move to line start and extend selection in visual mode."
  (interactive)
  (if meow-helix-visual-mode
      (progn
        (unless (region-active-p)
          (set-mark (point)))
        (beginning-of-line)
        (setq deactivate-mark nil))
    (beginning-of-line)))

(defun meow-helix-visual-line-end ()
  "Move to line end and extend selection in visual mode."
  (interactive)
  (if meow-helix-visual-mode
      (progn
        (unless (region-active-p)
          (set-mark (point)))
        (end-of-line)
        (setq deactivate-mark nil))
    (end-of-line)))

(defun meow-helix-visual-goto-file-start ()
  "Go to file start and extend selection in visual mode."
  (interactive)
  (if meow-helix-visual-mode
      (progn
        (unless (region-active-p)
          (set-mark (point)))
        (goto-char (point-min))
        (setq deactivate-mark nil))
    (meow-helix-goto-file-start)))

(defun meow-helix-visual-goto-file-end ()
  "Go to file end and extend selection in visual mode."
  (interactive)
  (if meow-helix-visual-mode
      (progn
        (unless (region-active-p)
          (set-mark (point)))
        (goto-char (point-max))
        (setq deactivate-mark nil))
    (meow-helix-goto-file-end)))

;;; Surround operations (Helix-style ms, md, mr)
(defvar meow-helix-surround-pairs
  '((?\( . ?\))
    (?\[ . ?\])
    (?\{ . ?\})
    (?\< . ?\>)
    (?\" . ?\")
    (?\' . ?\')
    (?\` . ?\`)
    (?* . ?*)     ; For markdown bold
    (?_ . ?_)     ; For markdown italic
    (?~ . ?~))    ; For strikethrough
  "Alist of opening and closing characters for surround operations.")

(defun meow-helix-surround-with-pair (open close)
  "Surround selection or current word with OPEN and CLOSE characters.
Works with multiple cursors if active."
  ;; If multiple cursors are active, apply to all
  (if (and (boundp 'multiple-cursors-mode)
           multiple-cursors-mode)
      (mc/execute-command-for-all-cursors
       (lambda ()
         (meow-helix-surround-single open close)))
    ;; Single cursor
    (meow-helix-surround-single open close)))

(defun meow-helix-surround-single (open close)
  "Surround single selection or word with OPEN and CLOSE characters."
  (if (region-active-p)
      (let ((start (region-beginning))
            (end (region-end))
            (text (buffer-substring (region-beginning) (region-end))))
        ;; Delete original text
        (delete-region start end)
        ;; Insert surrounded text
        (insert open text close)
        ;; Select the surrounded text (including delimiters)
        (goto-char start)
        (set-mark (+ end (length open) (length close)))
        (setq deactivate-mark nil))
    ;; No selection - select word first then surround
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (when bounds
        (goto-char (car bounds))
        (set-mark (cdr bounds))
        (setq deactivate-mark nil)
        (meow-helix-surround-single open close)))))

(defun meow-helix-get-surround-bounds ()
  "Get the bounds of surrounding pairs at point.
Returns (START . END) of the surrounding pair, or nil if none found."
  (save-excursion
    (let ((pairs meow-helix-surround-pairs)
          (orig-point (point))
          (start-pos nil)
          (end-pos nil)
          (best-start nil)
          (best-end nil))
      ;; Try each pair type
      (dolist (pair pairs)
        (let ((open (car pair))
              (close (cdr pair)))
          ;; Search backward for opening
          (save-excursion
            (when (search-backward (char-to-string open) nil t)
              (setq start-pos (point))
              ;; Search forward for matching closing from opening position
              (forward-char)
              (when (search-forward (char-to-string close) nil t)
                (setq end-pos (point))
                ;; Check if original point is between them
                (when (and (<= start-pos orig-point)
                           (<= orig-point end-pos)
                           ;; Keep the smallest valid surrounding
                           (or (not best-start)
                               (> start-pos best-start)))
                  (setq best-start start-pos
                        best-end end-pos)))))))
      (when best-start
        (cons best-start best-end)))))

(defun meow-helix-delete-surround ()
  "Delete surrounding pairs like Helix's md command.
Works with multiple cursors if active."
  (interactive)
  (if (and (boundp 'multiple-cursors-mode)
           multiple-cursors-mode)
      (mc/execute-command-for-all-cursors
       'meow-helix-delete-surround-single)
    (meow-helix-delete-surround-single)))

(defun meow-helix-delete-surround-single ()
  "Delete surrounding pair at point."
  (let ((bounds (meow-helix-get-surround-bounds)))
    (if bounds
        (let ((start (car bounds))
              (end (cdr bounds)))
          ;; Delete closing character first (so positions don't shift)
          (save-excursion
            (goto-char end)
            (delete-char -1))
          ;; Delete opening character
          (save-excursion
            (goto-char start)
            (delete-char 1))
          (unless (and (boundp 'multiple-cursors-mode)
                       multiple-cursors-mode)
            (message "Deleted surrounding pair")))
      (unless (and (boundp 'multiple-cursors-mode)
                   multiple-cursors-mode)
        (message "No surrounding pair found")))))

(defun meow-helix-replace-surround ()
  "Replace surrounding pairs like Helix's mr command.
Works with multiple cursors if active."
  (interactive)
  (let* ((char (read-char "Replace with: "))
         (pair (assoc char meow-helix-surround-pairs))
         (open-char (if pair char char))
         (close-char (if pair (cdr pair) char)))
    (if (and (boundp 'multiple-cursors-mode)
             multiple-cursors-mode)
        (mc/execute-command-for-all-cursors
         (lambda ()
           (meow-helix-replace-surround-single open-char close-char)))
      (meow-helix-replace-surround-single open-char close-char))))

(defun meow-helix-replace-surround-single (open-char close-char)
  "Replace surrounding pair at point with OPEN-CHAR and CLOSE-CHAR."
  (let ((bounds (meow-helix-get-surround-bounds)))
    (if bounds
        (let ((start (car bounds))
              (end (cdr bounds)))
          ;; Replace closing character first
          (save-excursion
            (goto-char end)
            (delete-char -1)
            (insert (char-to-string close-char)))
          ;; Replace opening character
          (save-excursion
            (goto-char start)
            (delete-char 1)
            (insert (char-to-string open-char)))
          (unless (and (boundp 'multiple-cursors-mode)
                       multiple-cursors-mode)
            (message "Replaced surrounding pair")))
      (unless (and (boundp 'multiple-cursors-mode)
                   multiple-cursors-mode)
        (message "No surrounding pair found")))))

;;; Text object selection (inside/around)
(defun meow-helix-find-surrounding-pair (open-char close-char)
  "Find the surrounding pair of OPEN-CHAR and CLOSE-CHAR.
Returns (START-OPEN . END-CLOSE) positions or nil."
  (let ((orig-point (point))
        (start-pos nil)
        (end-pos nil))
    (save-excursion
      ;; For same open/close chars (like quotes), use simpler logic
      (if (eq open-char close-char)
          ;; Handle quotes and other symmetric pairs
          (progn
            ;; Search backward for opening quote
            (when (search-backward (char-to-string open-char) nil t)
              (setq start-pos (point))
              ;; Search forward for closing quote from after the opening
              (forward-char)
              (when (search-forward (char-to-string close-char) nil t)
                (setq end-pos (point))
                ;; Verify cursor was between them
                (when (and (< start-pos orig-point)
                           (<= orig-point end-pos))
                  (cons start-pos end-pos)))))
        ;; For different open/close chars, handle nesting
        (catch 'found
          ;; Try progressively wider searches backward
          (goto-char orig-point)
          (while (search-backward (char-to-string open-char) nil t)
            (let ((potential-start (point))
                  (depth 1))
              ;; From this position, scan forward counting depth
              (save-excursion
                (forward-char) ; Skip the opening delimiter
                (while (and (> depth 0)
                            (re-search-forward 
                             (concat (regexp-quote (char-to-string open-char))
                                     "\\|" 
                                     (regexp-quote (char-to-string close-char)))
                             nil t))
                  (let ((found-char (char-before)))
                    (cond
                     ((eq found-char open-char) (setq depth (1+ depth)))
                     ((eq found-char close-char) (setq depth (1- depth))))))
                ;; If depth is 0, we found matching close
                (when (= depth 0)
                  (let ((potential-end (point)))
                    ;; Check if cursor was inside this pair
                    (when (and (< potential-start orig-point)
                               (<= orig-point potential-end))
                      (throw 'found (cons potential-start potential-end)))))))))))))

(defun meow-helix-select-inside-pair (open-char close-char)
  "Select inside the pair defined by OPEN-CHAR and CLOSE-CHAR."
  (let ((bounds (meow-helix-find-surrounding-pair open-char close-char)))
    (when bounds
      (goto-char (1+ (car bounds))) ; Move past opening delimiter
      (set-mark (1- (cdr bounds)))   ; Move before closing delimiter
      (setq deactivate-mark nil))))

(defun meow-helix-select-around-pair (open-char close-char)
  "Select around the pair defined by OPEN-CHAR and CLOSE-CHAR, including delimiters."
  (let ((bounds (meow-helix-find-surrounding-pair open-char close-char)))
    (when bounds
      (goto-char (car bounds))
      (set-mark (cdr bounds))
      (setq deactivate-mark nil))))

(defun meow-helix-select-text-object (type char)
  "Select text object of TYPE ('inside or 'around) for CHAR."
  (let ((pair (assoc char meow-helix-surround-pairs)))
    (cond
     ;; Paired delimiters
     (pair
      (if (eq type 'inside)
          (meow-helix-select-inside-pair char (cdr pair))
        (meow-helix-select-around-pair char (cdr pair))))
     ;; Word object
     ((eq char ?w)
      (let ((bounds (bounds-of-thing-at-point 'word)))
        (when bounds
          (goto-char (car bounds))
          (set-mark (cdr bounds))
          (setq deactivate-mark nil))))
     ;; WORD object (whitespace delimited)
     ((eq char ?W)
      (let ((start (save-excursion
                     (skip-chars-backward "^ \t\n")
                     (point)))
            (end (save-excursion
                   (skip-chars-forward "^ \t\n")
                   (point))))
        (goto-char start)
        (set-mark end)
        (setq deactivate-mark nil)))
     ;; Paragraph
     ((eq char ?p)
      (if (eq type 'inside)
          (mark-paragraph)
        (progn
          (mark-paragraph)
          ;; Extend to include blank lines
          (when (region-active-p)
            (goto-char (region-end))
            (skip-chars-forward " \t\n")
            (set-mark (region-beginning))))))
     ;; Function (tree-sitter aware)
     ((eq char ?f)
      (when (and (fboundp 'treesit-available-p)
                 (treesit-available-p)
                 (treesit-language-at (point)))
        (let ((node (treesit-node-at (point))))
          ;; Find function node
          (while (and node
                      (not (member (treesit-node-type node)
                                   '("function_definition" "method_definition" 
                                     "function_declaration" "function"))))
            (setq node (treesit-node-parent node)))
          (when node
            (goto-char (treesit-node-start node))
            (set-mark (treesit-node-end node))
            (setq deactivate-mark nil))))))))

(defun meow-helix-match-inside ()
  "Select inside text object (Helix mi)."
  (interactive)
  (let ((char (read-char "Inside: ")))
    (if (and (boundp 'multiple-cursors-mode)
             multiple-cursors-mode)
        (mc/execute-command-for-all-cursors
         (lambda () (meow-helix-select-text-object 'inside char)))
      (meow-helix-select-text-object 'inside char))))

(defun meow-helix-match-around ()
  "Select around text object (Helix ma)."
  (interactive)
  (let ((char (read-char "Around: ")))
    (if (and (boundp 'multiple-cursors-mode)
             multiple-cursors-mode)
        (mc/execute-command-for-all-cursors
         (lambda () (meow-helix-select-text-object 'around char)))
      (meow-helix-select-text-object 'around char))))

(defvar meow-helix-surround-mode-map
  (let ((map (make-sparse-keymap)))
    ;; ms commands - surround with pairs
    (define-key map "s" 'meow-helix-surround-prompt)
    (define-key map "(" (lambda () (interactive) (meow-helix-surround-with-pair "(" ")")))
    (define-key map ")" (lambda () (interactive) (meow-helix-surround-with-pair "(" ")")))
    (define-key map "[" (lambda () (interactive) (meow-helix-surround-with-pair "[" "]")))
    (define-key map "]" (lambda () (interactive) (meow-helix-surround-with-pair "[" "]")))
    (define-key map "{" (lambda () (interactive) (meow-helix-surround-with-pair "{" "}")))
    (define-key map "}" (lambda () (interactive) (meow-helix-surround-with-pair "{" "}")))
    (define-key map "<" (lambda () (interactive) (meow-helix-surround-with-pair "<" ">")))
    (define-key map ">" (lambda () (interactive) (meow-helix-surround-with-pair "<" ">")))
    (define-key map "\"" (lambda () (interactive) (meow-helix-surround-with-pair "\"" "\"")))
    (define-key map "'" (lambda () (interactive) (meow-helix-surround-with-pair "'" "'")))
    (define-key map "`" (lambda () (interactive) (meow-helix-surround-with-pair "`" "`")))
    (define-key map "*" (lambda () (interactive) (meow-helix-surround-with-pair "*" "*")))
    (define-key map "_" (lambda () (interactive) (meow-helix-surround-with-pair "_" "_")))
    (define-key map "~" (lambda () (interactive) (meow-helix-surround-with-pair "~" "~")))
    ;; md command - delete surround
    (define-key map "d" 'meow-helix-delete-surround)
    ;; mr command - replace surround
    (define-key map "r" 'meow-helix-replace-surround)
    ;; mi command - select inside
    (define-key map "i" 'meow-helix-match-inside)
    ;; ma command - select around
    (define-key map "a" 'meow-helix-match-around)
    map)
  "Keymap for Helix-style surround mode (ms/md/mr/mi/ma).")

(defun meow-helix-surround-prompt ()
  "Prompt for character and surround with corresponding pair."
  (interactive)
  (let* ((char (read-char "Surround with: "))
         (pair (assoc char meow-helix-surround-pairs))
         (open (if pair (char-to-string char) (char-to-string char)))
         (close (if pair (char-to-string (cdr pair)) (char-to-string char))))
    (meow-helix-surround-with-pair open close)))

(defun meow-helix-match-mode ()
  "Enter Helix-style match/surround mode (m prefix)."
  (interactive)
  (message "Match mode: i=inside, a=around, s=surround, d=delete, r=replace")
  (set-transient-map meow-helix-surround-mode-map))

;;; Tree-sitter smart selection
(defvar-local meow-helix-selection-history nil
  "History of selections for shrinking back.")

(defun meow-helix-expand-selection ()
  "Expand selection to parent syntax node using tree-sitter (Helix Alt-o)."
  (interactive)
  (if (and (fboundp 'treesit-available-p)
           (treesit-available-p)
           (treesit-language-at (point)))
      ;; Tree-sitter mode
      (let* ((start (if (region-active-p) (region-beginning) (point)))
             (end (if (region-active-p) (region-end) (point))))
        ;; Save current selection to history before expanding
        (when (region-active-p)
          (push (cons start end) meow-helix-selection-history))
        
        ;; Get all nodes at current position and find best parent
        (let ((node (treesit-node-at start))
              (target-node nil))
          
          (when node
            ;; If no selection, start by selecting current node
            (if (not (region-active-p))
                (progn
                  (setq meow-helix-selection-history nil)
                  (setq target-node node))
              ;; We have a selection - find smallest containing node then expand
              (let ((current node))
                ;; Walk up to find smallest node containing selection
                (while (and current
                            (not (and (>= start (treesit-node-start current))
                                      (<= end (treesit-node-end current)))))
                  (setq current (treesit-node-parent current)))
                ;; If we found containing node and it matches exactly, go to parent
                (when current
                  (if (and (= start (treesit-node-start current))
                           (= end (treesit-node-end current)))
                      ;; Exact match - expand to parent
                      (setq target-node (treesit-node-parent current))
                    ;; Not exact match - select this containing node
                    (setq target-node current)))))
            
            ;; Apply selection if we have a target
            (when target-node
              (goto-char (treesit-node-start target-node))
              (set-mark (treesit-node-end target-node))
              (setq deactivate-mark nil)))))
    ;; Fallback for non-tree-sitter buffers
    (cond
     ((not (region-active-p))
      ;; No selection - select word
      (let ((bounds (bounds-of-thing-at-point 'word)))
        (when bounds
          (goto-char (car bounds))
          (set-mark (cdr bounds)))))
     ((save-excursion
        (goto-char (region-beginning))
        (and (bolp)
             (goto-char (region-end))
             (eolp)))
      ;; Full line selected - expand to paragraph
      (mark-paragraph))
     (t
      ;; Partial selection - expand to line
      (beginning-of-line)
      (set-mark (point))
      (end-of-line)))))

(defun meow-helix-shrink-selection ()
  "Shrink selection by popping from history, finding child node, or jumping to node start."
  (interactive)
  (cond
   ;; If we have history, use it
   (meow-helix-selection-history
    (let ((prev (pop meow-helix-selection-history)))
      (goto-char (car prev))
      (set-mark (cdr prev))
      (setq deactivate-mark nil)))
   
   ;; If no selection but in tree-sitter buffer, jump to start of current node
   ((and (fboundp 'treesit-available-p)
         (treesit-available-p)
         (treesit-language-at (point))
         (not (region-active-p)))
    (let ((node (treesit-node-at (point))))
      (when node
        ;; Find the smallest named node at point
        (while (and node
                    (not (treesit-node-check node 'named)))
          (setq node (treesit-node-parent node)))
        (when node
          (goto-char (treesit-node-start node))))))
   
   ;; If selection exists, try tree-sitter shrinking
   ((and (fboundp 'treesit-available-p)
         (treesit-available-p)
         (treesit-language-at (point))
         (region-active-p))
    ;; Find first child of current selection
    (let* ((start (region-beginning))
           (end (region-end))
           (node (treesit-node-at start)))
      ;; Find node matching our selection
      (while (and node
                  (or (< (treesit-node-start node) start)
                      (> (treesit-node-end node) end)))
        (setq node (treesit-node-parent node)))
      
      ;; Get first named child
      (when node
        (let ((child nil))
          (dotimes (i (treesit-node-child-count node))
            (unless child
              (let ((c (treesit-node-child node i)))
                (when (and c (treesit-node-check c 'named))
                  (setq child c)))))
          
          (when child
            (goto-char (treesit-node-start child))
            (set-mark (treesit-node-end child))
            (setq deactivate-mark nil))))))
    ;; Fallback for non-tree-sitter buffers
    (when (region-active-p)
      (let ((start (region-beginning))
            (end (region-end)))
        ;; If we have a paragraph, shrink to line
        (if (and (save-excursion
                   (goto-char start)
                   (bolp))
                 (save-excursion
                   (goto-char end)
                   (eolp))
                 (> (count-lines start end) 1))
            (progn
              (goto-char start)
              (set-mark (point))
              (end-of-line)
              (setq deactivate-mark nil))
          ;; Otherwise shrink to word
          (let ((word-bounds (save-excursion
                               (goto-char start)
                               (bounds-of-thing-at-point 'word))))
            (when word-bounds
              (goto-char (car word-bounds))
              (set-mark (cdr word-bounds))
              (setq deactivate-mark nil))))))))

;; Modified escape to also exit visual mode
(defun meow-helix-escape ()
  "Cancel selection and exit visual mode if active."
  (interactive)
  (when meow-helix-visual-mode
    (meow-helix-visual-mode-off))
  (meow-cancel-selection))

;; Modeline indicator for visual mode
(defun meow-helix-modeline-indicator ()
  "Return indicator string for modeline."
  (cond
   (meow-helix-visual-mode " VISUAL")
   ((meow-insert-mode-p) " INSERT")
   ((meow-normal-mode-p) " NORMAL")
   (t "")))

;; Add to modeline
(setq-default mode-line-format
              (append mode-line-format
                      '((:eval (meow-helix-modeline-indicator)))))

(defun meow-setup-helix ()
  "Setup meow to mimic Helix editor keybindings."
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  
  (setq meow-use-cursor-position-hack t)
  (setq meow-use-enhanced-selection-effect t)
  
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))

  (meow-leader-define-key
   '("?" . meow-cheatsheet))

  (meow-normal-define-key
   '("h" . meow-helix-left)
   '("j" . meow-helix-visual-next)
   '("k" . meow-helix-visual-prev)
   '("l" . meow-helix-right)
   
   '("w" . meow-helix-visual-next-word)
   '("W" . meow-next-symbol)
   '("b" . meow-helix-visual-back-word)
   '("B" . meow-back-symbol)
   '("e" . meow-helix-visual-next-word)
   '("E" . meow-next-symbol)
   
   '("f" . meow-helix-find-char)
   '("t" . meow-helix-find-till)
   '("F" . meow-find)
   '("T" . meow-till)
   
   '("/" . isearch-forward)
   '("?" . isearch-backward)
   '("n" . meow-helix-search-next)
   '("N" . meow-helix-search-prev)
   '("*" . meow-helix-search-symbol-at-point)
   
   '("m" . meow-helix-match-mode)
   '("%" . meow-helix-select-whole-file)  ; Select whole file like Helix
   
   '("x" . meow-line)
   '("X" . meow-helix-extend-line)
   
   '("c" . meow-change)
   ;; '("C" . meow-helix-duplicate)  ; Replaced with cursor-below
   '("M-c" . meow-helix-toggle-comment)  ; Comment toggle (Alt-c, since C-c conflicts)
   '("d" . meow-helix-delete)  ; Simple delete without extending selection
   '("D" . meow-kill-whole-line)
   
   '("y" . meow-helix-copy)
   '("Y" . kill-ring-save)
   '("p" . meow-helix-paste-after)
   '("P" . meow-helix-paste-before)

   '("i" . meow-insert)
   '("I" . meow-insert-at-line-start)  ; Should enter insert mode at line start
   '("a" . meow-append)
   '("A" . meow-append-at-line-end)     ; Should enter insert mode at line end
   '("o" . meow-open-below)
   '("O" . meow-open-above)
   
   '("u" . undo)
   '("U" . undo-redo)
   
   '("r" . meow-replace-char)
   '("R" . meow-replace)
   
   ;; '("s" . meow-helix-select-word)  ; Replaced with select-all-matches
   '("S" . meow-helix-extend-word)
   
   '("v" . meow-helix-toggle-visual-mode)  ; Visual/extend mode like Helix
   '("V" . meow-kmacro-lines)
   
   '(">" . meow-helix-indent)
   '("<" . meow-helix-unindent)
   '("=" . indent-region)
   
   '("J" . meow-helix-join-lines)
   ;; K in Helix is just move up, no special shift behavior
   '("K" . meow-prev)
   
   '("g" . meow-helix-goto-mode)
   '("G" . meow-grab)
   
   '("z" . meow-pop-selection)
   '("Z" . meow-pop-all-selection)
   
   '(":" . meow-helix-ex-command)  ; Helix-style command mode
   '("&" . meow-query-replace)
   '(";" . meow-reverse)
   '("," . meow-helix-keep-primary-cursor)  ; Context-aware: collapse cursors or inner-thing
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   
   ;; Tree-sitter smart selection
   '("M-o" . meow-helix-expand-selection)   ; Alt-o - expand to parent node
   '("M-i" . meow-helix-shrink-selection)   ; Alt-i - shrink to child node
   
   '("C-a" . meow-helix-select-all)
   '("C-b" . meow-helix-page-up)
   '("C-f" . meow-helix-page-down)
   ;; '("C-u" . meow-helix-half-page-up)  ; Commented out to restore C-u as universal argument
   '("C-d" . meow-helix-half-page-down)
   '("C-w" . meow-helix-window-mode)
   
   '("SPC" . meow-helix-space-mode)
   
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("<escape>" . meow-helix-escape)))  ; Modified to exit visual mode too


(use-package meow
  :ensure t
  :init
  (setq meow-use-clipboard t)
  (setq meow-selection-command-fallback
        '((meow-change . meow-change-char)
          (meow-kill . meow-delete)
          (meow-cancel-selection . keyboard-quit)
          (meow-pop-selection . meow-pop-grab)
          (meow-beacon-change . meow-beacon-change-char)))
  :config
  (meow-setup-helix)
  (meow-global-mode 1)
  
  ;; Terminal mode configuration
  (add-to-list 'meow-mode-state-list '(vterm-mode . insert))
  (add-to-list 'meow-mode-state-list '(eshell-mode . insert)))

;;; Helix-style multiple cursor support
(use-package multiple-cursors
  :ensure t
  :after meow
  :config
  ;; Improve visual feedback
  (setq mc/always-run-for-all t)  ; Run commands for all cursors by default
  
  ;; Custom faces for better visibility
  (set-face-attribute 'mc/cursor-face nil
                      :background "#ff6c6b"
                      :foreground "black")
  (set-face-attribute 'mc/cursor-bar-face nil
                      :background "#ff6c6b"
                      :height 1)
  
  (defun meow-helix-cursor-below ()
    "Add cursor to next line, like Helix's C command."
    (interactive)
    (if (region-active-p)
        (let ((col (- (point) (line-beginning-position))))
          (mc/create-fake-cursor-at-point)
          (forward-line 1)
          (beginning-of-line)
          (forward-char col))
      (progn
        (meow-mark-word 1)
        (mc/mark-next-like-this 1))))
  
  (defun meow-helix-cursor-above ()
    "Add cursor to previous line, like Helix's Alt-C command."
    (interactive)
    (if (region-active-p)
        (let ((col (- (point) (line-beginning-position))))
          (mc/create-fake-cursor-at-point)
          (forward-line -1)
          (beginning-of-line)
          (forward-char col))
      (progn
        (meow-mark-word 1)
        (mc/mark-previous-like-this 1))))
  
  (defun meow-helix-select-all-matches ()
    "Select all matches in buffer or selection, like Helix's s command."
    (interactive)
    (if (region-active-p)
        ;; Use mc/mark-all-matches-in-region which prompts for pattern
        (call-interactively 'mc/mark-all-in-region)
      ;; No selection - select word and find all matches
      (progn
        (meow-mark-word 1)
        (mc/mark-all-like-this))))
  
  (defun meow-helix-split-selection-on-newlines ()
    "Split selection on newlines, like Helix's Alt-s."
    (interactive)
    (when (region-active-p)
      (mc/edit-lines)))
  
  (defun meow-helix-align-cursors ()
    "Align all cursors to the same column."
    (interactive)
    (mc/vertical-align))
  
  (defun meow-helix-keep-primary-cursor ()
    "Keep only primary cursor, like Helix's comma command.
If multiple cursors are active, collapse to single cursor.
Otherwise, use default comma behavior (inner-of-thing)."
    (interactive)
    (if (> (mc/num-cursors) 1)
        ;; Multiple cursors active - collapse to primary
        (mc/keyboard-quit)
      ;; Single cursor - use normal comma behavior (select inner of thing)
      (call-interactively 'meow-inner-of-thing)))
  
  ;; Add keybindings for multiple cursor commands
  (define-key meow-normal-state-keymap (kbd "C") 'meow-helix-cursor-below)
  (define-key meow-normal-state-keymap (kbd "M-C") 'meow-helix-cursor-above)
  (define-key meow-normal-state-keymap (kbd "s") 'meow-helix-select-all-matches)
  (define-key meow-normal-state-keymap (kbd "M-s") 'meow-helix-split-selection-on-newlines)
  (define-key meow-normal-state-keymap (kbd "&") 'meow-helix-align-cursors))
