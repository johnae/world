(defun kill-other-buffers ()
  "Kill all other buffers except the current one."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun delete-other-windows-and-buffers ()
  "Kill all other buffers and windows except the current one."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
  (delete-other-windows)
  )

(defun jae/project-name ()
  (file-name-nondirectory (directory-file-name (project-root (project-current t))))
  )

(defun project-buffer (name)
  "Name for project term buffer."
  (interactive)
  (concat "*" (jae/project-name) "-" name "*")
  )

(defun find-window-by-name (name)
  "Find the window with the given NAME parameter."
  (let ((window (seq-find (lambda (w)
			    (eq (window-parameter w 'name) name))
			  (window-list)))) window)
  )

(defun select-window-by-name (name)
  "Select the window with the given NAME parameter."
  (interactive)
  (let ((window (find-window-by-name name)))
    (when window
      (select-window window))))

(defun zoom-toggle-window-by-name (name)
  "Toggle fullscreen of window with name."
  (interactive)
  (message (format "len: %s" (length (window-list))))
  (if (eq (length (window-list)) 1)
      (progn
	(let ((window (find-window-by-name name)))
	  (winner-undo)
	  (if (eq window nil) (zoom-toggle-window-by-name name))
	  )
	)
    (progn
        (select-window-by-name name)
	(delete-other-windows)
      )
    )
  )


(defun jae/layout-main-term ()
  "Set up a custom layout with main area and bottom terminal."
  (interactive)
  (delete-other-windows)
  (set-window-parameter (selected-window) 'name 'main)

  (let* (
         (main-window (select-window-by-name 'main))
	 (bottom-window-height-percent 0.20)
	 (bottom-window-height (- (round (* (frame-height) bottom-window-height-percent))))
	 (bottom-window (split-window main-window bottom-window-height nil))
	 )
       
       (set-window-parameter bottom-window 'name 'term)
       (select-window-by-name 'term)
       (let ((buffer (get-buffer-create (project-prefixed-buffer-name "term"))))
	 (set-window-buffer (selected-window) buffer)
	 (switch-to-buffer buffer)
	 (eat-mode)
	 (let* ((default-directory (project-root (project-current t))))
	   (eat-exec buffer "term" "nu" nil nil)
	   ))
       (select-window-by-name 'main)))

(defun jae/layout-main-side-term-bottom-term (sidecmd)
  "Set up a custom layout with main area, side process and bottom terminal."
  (interactive)
  (delete-other-windows)
  (set-window-parameter (selected-window) 'name 'main)
  (let* (
	 (main-window (select-window-by-name 'main))
	 (side-window-width-percent 0.25)
	 (bottom-window-height-percent 0.20)

	 (side-window-width (- (round (* (frame-width) side-window-width-percent))))
	 (bottom-window-height (- (round (* (frame-height) bottom-window-height-percent))))

	 (side-window (split-window main-window side-window-width t))
	 (bottom-window (split-window main-window bottom-window-height nil))
	)

       (set-window-parameter side-window 'name 'side)
       (set-window-parameter bottom-window 'name 'term)

       (select-window-by-name 'side)
       (let ((buffer (get-buffer-create (project-prefixed-buffer-name "side"))))
	 (set-window-buffer (selected-window) buffer)
	 (switch-to-buffer buffer)
	 (eat-mode)
	 (let* ((default-directory (project-root (project-current t))))
	   (eat-exec buffer "side" "bash" nil (list "-c" sidecmd))
	  ))

       (select-window-by-name 'term)
       (let ((buffer (get-buffer-create (project-prefixed-buffer-name "term"))))
	 (set-window-buffer (selected-window) buffer)
	 (switch-to-buffer buffer)
	 (eat-mode)
	 (let* ((default-directory (project-root (project-current t))))
	   (eat-exec buffer "term" "nu" nil nil)
	   ))
       (select-window-by-name 'main)))

(defun jae/layout-main-side-x2-term-bottom-term (topsidecmd bottomsidecmd)
  "Set up a custom layout with main area, top side process, bottom side process and bottom terminal."
  (interactive)
  (delete-other-windows)
  (set-window-parameter (selected-window) 'name 'main)

  (let* (
	 (main-window (select-window-by-name 'main))
	 (side-window-width-percent 0.25)
	 (bottom-window-height-percent 0.20)

	 (side-window-width (- (round (* (frame-width) side-window-width-percent))))
	 (bottom-window-height (- (round (* (frame-height) bottom-window-height-percent))))

	 (side-top-window (split-window main-window side-window-width t))
	 (side-bottom-window (split-window side-top-window (/ (window-height) 2)))
	 (bottom-window (split-window main-window bottom-window-height nil))
	 )

       (set-window-parameter side-top-window 'name 'topside)
       (set-window-parameter side-bottom-window 'name 'bottomside)
       (set-window-parameter bottom-window 'name 'term)

       (select-window-by-name 'topside)
       (let ((buffer (get-buffer-create (project-prefixed-buffer-name "topside"))))
	 (set-window-buffer (selected-window) buffer)
	 (switch-to-buffer buffer)
	 (eat-mode)
	 (let* ((default-directory (project-root (project-current t))))
	   (eat-exec buffer "topside" "bash" nil (list "-c" topsidecmd))
	   ))
       
       (select-window-by-name 'bottomside)
       (let ((buffer (get-buffer-create (project-prefixed-buffer-name "bottomside"))))
	 (set-window-buffer (selected-window) buffer)
	 (switch-to-buffer buffer)
	 (eat-mode)
	 (let* ((default-directory (project-root (project-current t))))
	   (eat-exec buffer "bottomside" "bash" nil (list "-c" topsidecmd))
	  ))

       (select-window-by-name 'term)
       (let ((buffer (get-buffer-create (project-prefixed-buffer-name "term"))))
	 (set-window-buffer (selected-window) buffer)
	 (switch-to-buffer buffer)
	 (eat-mode)
	 (let* ((default-directory (project-root (project-current t))))
	   (eat-exec buffer "term" "nu" nil nil)
	   ))
       (select-window-by-name 'main)))

(defun rotate-visible-buffers ()
  "Rotate buffers among all visible windows in the current frame."
  (interactive)
  ;; Get all visible windows and their buffers
  (let* ((windows (window-list))
         (buffers (mapcar #'window-buffer windows))) ;; List of buffers in the current windows
    ;; Rotate buffers list by moving the last buffer to the front
    (setq buffers (append (last buffers) (butlast buffers)))
    ;; Set each window's buffer to the next one in the rotated list
    (cl-mapc #'set-window-buffer windows buffers)))
