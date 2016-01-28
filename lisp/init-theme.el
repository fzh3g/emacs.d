;(use-package dracula-theme
;  :init
;  (add-hook 'after-init-hook
;            '(lambda ()
;               (load-theme 'dracula t))))

(use-package solarized-theme
  :init
  (add-hook 'after-init-hook
            '(lambda ()
               (setq solarized-scale-org-headlines nil)
               (setq solarized-use-more-italic t)
               (load-theme 'solarized-dark t)))
  :config
  (progn
    (defun light ()
      "Activate a light color theme."
      (interactive)
      (load-theme 'solarized-light t))
    (defun dark ()
      "Activate a dark color theme."
      (interactive)
      (load-theme 'solarized-dark t))))

(provide 'init-theme)
