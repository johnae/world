(defun split-window-right-and-move ()
  "Split the window vertically and move to the new window."
  (interactive)
  (split-window-right)       ; Split the window vertically
  (other-window 1))          ; Move to the new split window

(defun split-window-below-and-move ()
  "Split the window vertically and move to the new window."
  (interactive)
  (split-window-right)       ; Split the window vertically
  (other-window 1))          ; Move to the new split window

(defun insert-at-end-of-line ()
  "Move to end of line and enter insert mode."
  (interactive)
  (end-of-line)
  (meow-insert))

(defun insert-at-start-of-line ()
  "Move to start of line and enter insert mode."
  (interactive)
  (beginning-of-line-text)
  (meow-insert))

(defun paste-below ()
  "Paste copied text below."
  (interactive)
  (next-line)
  (meow-yank)
  (previous-line))

(keymap-global-set "C-t" 'project-eshell)

(defun meow-setup-helix-style ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   ;; Basic motions
   '("j" . meow-next)
   '("h" . meow-prev)
   '("<escape>" . ignore))

  (meow-leader-define-key
   ;; Quick access to common commands
   '("f" . project-find-file)

   '("i" . beginning-of-buffer)
   '("e" . end-of-buffer)
   '("b" . project-switch-to-buffer)   
   '("w" . save-buffer)
   '("k" . kill-buffer)
   '("1" . delete-other-windows)
   '("s" . split-window-below-and-move)
   '("v" . split-window-right-and-move)
   '("q" . delete-window)

   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))

  ;; Set Normal Mode Bindings
  (meow-normal-define-key
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
   '(";" . meow-reverse)
   ;; Basic motions
   '("h" . meow-left)
   '("j" . meow-next)
   '("k" . meow-prev)
   '("l" . meow-right)
   ;; Word motions
   '("w" . meow-next-word)
   '("b" . meow-back-word)
   ;; Searching
   '("f" . meow-find)
   '("s" . meow-search)
   ;; Start/end of line
   '("0" . meow-beginning-of-line)
   '("$" . meow-end-of-line)
   ;; Parentheses matching
   '("%" . meow-find-match)
   ;; Scrolling
   '("u" . scroll-down-command)
   '("d" . scroll-up-command)

   '("c" . meow-change)

   '("C" . meow-comment)

   '("r" . meow-replace-char)
   '("R" . meow-replace)

   '(">" . indent-rigidly-right)
   '("<" . indent-rigidly-left)

   '("i" . meow-insert)
   '("I" . insert-at-start-of-line)
   '("A" . insert-at-end-of-line)
   '("o" . meow-open-below)
   '("O" . meow-open-above)
   ;; Selection and deletion
   '("G" . meow-grab)
   '("x" . meow-line)
   '("d" . meow-kill)
   ;; Undo/redo
   '("u" . undo)
   '("U" . undo-redo)
   ;; Copy/yank
   '("y" . meow-save)
   '("Y" . kill-ring-save)
   '("p" . paste-below)
   ;; Additional motions and selections
   '("n" . meow-mark-word)
   '("N" . meow-mark-symbol)
   '("W" . meow-mark-outer-word)
   '("L" . meow-mark-outer-line)
   '("g" . meow-goto-line)
   ;; Exiting insert mode
   '("C-g" . meow-cancel)
   '("<escape>" . ignore)
   ))

(use-package meow
  :ensure t
  :config
  (meow-setup-helix-style)
  (meow-global-mode 1))
