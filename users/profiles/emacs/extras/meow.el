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

(defvar meow-helix-goto-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'meow-helix-goto-file-start)
    (define-key map "e" 'meow-helix-goto-file-end)
    (define-key map "h" 'meow-helix-goto-line-start)
    (define-key map "l" 'meow-helix-goto-line-end)
    (define-key map "s" 'meow-helix-goto-line-start)
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

(defvar meow-helix-match-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "m" 'meow-helix-match-brackets)
    (define-key map "s" (lambda () (interactive) (surround-mode 1)))
    (define-key map "(" (lambda () (interactive) (insert-pair nil ?\( ?\))))
    (define-key map "[" (lambda () (interactive) (insert-pair nil ?\[ ?\])))
    (define-key map "{" (lambda () (interactive) (insert-pair nil ?\{ ?\})))
    (define-key map "'" (lambda () (interactive) (insert-pair nil ?\' ?\' )))
    (define-key map "\"" (lambda () (interactive) (insert-pair nil ?\" ?\")))
    map)
  "Keymap for Helix-style match mode.")

(defun meow-helix-match-mode ()
  "Enter Helix-style match mode (m prefix)."
  (interactive)
  (set-transient-map meow-helix-match-mode-map))

(defvar meow-helix-space-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "f" 'project-find-file)
    (define-key map "F" 'find-file)
    (define-key map "b" 'switch-to-buffer)
    (define-key map "B" 'ibuffer)
    (define-key map "s" 'save-buffer)
    (define-key map "S" 'save-some-buffers)
    (define-key map "q" 'save-buffers-kill-emacs)
    (define-key map "Q" 'kill-emacs)
    (define-key map "w" 'meow-helix-window-mode)  ;; Changed to enter window mode
    (define-key map "W" 'delete-other-windows)
    (define-key map "v" 'split-window-right)
    (define-key map "h" 'split-window-below)
    (define-key map "o" 'other-window)
    (define-key map "k" 'kill-current-buffer)
    (define-key map "K" 'kill-buffer)
    (define-key map "r" 'query-replace)
    (define-key map "R" 'query-replace-regexp)
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
   '("h" . meow-left)
   '("j" . meow-next)
   '("k" . meow-prev)
   '("l" . meow-right)
   
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   
   '("f" . meow-helix-find-char)
   '("t" . meow-helix-find-till)
   '("F" . meow-find)
   '("T" . meow-till)
   
   '("/" . isearch-forward)
   '("?" . isearch-backward)
   '("n" . meow-helix-search-next)
   '("N" . meow-helix-search-prev)
   '("*" . isearch-forward-symbol-at-point)
   
   '("m" . meow-helix-match-mode)
   '("%" . meow-helix-match-brackets)
   
   '("x" . meow-line)
   '("X" . meow-helix-extend-line)
   
   '("c" . meow-change)
   '("C" . meow-helix-duplicate)
   '("d" . meow-kill)
   '("D" . meow-kill-whole-line)
   
   '("y" . meow-helix-copy)
   '("Y" . kill-ring-save)
   '("p" . meow-helix-paste-after)
   '("P" . meow-helix-paste-before)

   '("i" . meow-insert)
   '("I" . meow-helix-goto-line-start)
   '("a" . meow-append)
   '("A" . meow-helix-goto-line-end)
   '("o" . meow-open-below)
   '("O" . meow-open-above)
   
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("C-r" . undo-redo)
   
   '("r" . meow-replace-char)
   '("R" . meow-replace)
   
   '("s" . meow-helix-select-word)
   '("S" . meow-helix-extend-word)
   
   '("v" . meow-visit)
   '("V" . meow-kmacro-lines)
   
   '(">" . meow-helix-indent)
   '("<" . meow-helix-unindent)
   '("=" . indent-region)
   
   '("J" . meow-helix-join-lines)
   '("K" . meow-kill-whole-line)
   
   '("g" . meow-helix-goto-mode)
   '("G" . meow-grab)
   
   '("z" . meow-pop-selection)
   '("Z" . meow-pop-all-selection)
   
   '("&" . meow-query-replace)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   
   '("C-a" . meow-helix-select-all)
   '("C-b" . meow-helix-page-up)
   '("C-f" . meow-helix-page-down)
   '("C-u" . meow-helix-half-page-up)
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
   '("<escape>" . meow-cancel-selection)))


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
