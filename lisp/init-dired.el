(require 'dired+)

;; search file name only when focus is over file
(setq-default dired-isearch-filenames 'dwim)
(setq-default diredp-hide-details-initially-flag nil)
(setq dired-dwim-target t)
(eval-after-load 'dired
  '(progn
     (when (fboundp 'global-dired-hide-details-mode)
       (global-dired-hide-details-mode -1))
     (define-key dired-mode-map "/" 'dired-isearch-filenames)
     (define-key dired-mode-map (kbd "SPC") 'ace-jump-mode)
     (setq dired-recursive-deletes 'always)
     (define-key dired-mode-map [mouse-2] 'dired-find-file)))

(provide 'init-dired)
