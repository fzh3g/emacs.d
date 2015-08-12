;;----------------------------------------------------------------------------
;; Nicer naming of buffers for files with identical names
;;----------------------------------------------------------------------------
(use-package uniquify
  :init
  (progn
    (setq uniquify-buffer-name-style 'reverse)
    (setq uniquify-separator " • ")
    (setq uniquify-after-kill-buffer-p t)
    (setq uniquify-ignore-buffers-re "^\\*")))

(provide 'init-uniquify)
