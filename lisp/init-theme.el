;(use-package solarized-theme
;  :init
;  (add-hook 'after-init-hook
;            '(lambda ()
;               (setq solarized-scale-org-headlines nil)
;               (setq solarized-use-more-italic t)
;               (load-theme 'solarized-dark t)))
;  :config
;  (progn
;    (defun light-theme ()
;      "Activate a light color theme."
;      (interactive)
;      (load-theme 'solarized-light t))
;    (defun dark-theme ()
;      "Activate a dark color theme."
;      (interactive)
;      (load-theme 'solarized-dark t))))

(use-package color-theme-sanityinc-tomorrow
  :init
  (add-hook 'after-init-hook
            '(lambda ()
               (load-theme 'sanityinc-tomorrow-night t)))
  :config
  (progn
    (defun light-theme ()
      "Load a light color theme."
      (interactive)
      (load-theme 'sanityinc-tomorrow-day t))
    (defun dark-theme ()
      "Load a dark color theme."
      (interactive)
      (load-theme 'sanityinc-tomorrow-night t))))

(provide 'init-theme)
