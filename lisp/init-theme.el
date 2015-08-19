;(use-package cyberpunk-theme
;  :init
;  (add-hook 'after-init-hook
;            '(lambda ()
;               (load-theme 'cyberpunk t))))

(use-package color-theme-sanityinc-solarized
  :init
  (defun light ()
    "Activate a light color theme."
    (interactive)
    (color-theme-sanityinc-solarized-light))

  (defun dark ()
    "Activate a dark color theme."
    (interactive)
    (color-theme-sanityinc-solarized-dark))

  (add-hook 'after-init-hook 'color-theme-sanityinc-solarized-dark))

(provide 'init-theme)
