(use-package dired+
  :init
  (progn
    (setq dired-recursive-deletes 'always)
    (setq dired-isearch-filenames 'dwim)
    (setq diredp-hide-details-initially-flag t)
    (setq dired-dwim-target t))
  :config
  (progn
    (when (fboundp 'global-dired-hide-details-mode)
      (global-dired-hide-details-mode -1))
    (define-key dired-mode-map "/" 'dired-isearch-filenames)
    (define-key dired-mode-map (kbd "SPC") 'avy-goto-subword-1)
    (define-key dired-mode-map [mouse-2] 'dired-find-file)))

(provide 'init-dired)
