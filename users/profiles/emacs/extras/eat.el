(use-package eat
  :ensure t
  )

(setq process-adaptive-read-buffering nil)
(setq read-process-output-max (* 4 1024 1024))
