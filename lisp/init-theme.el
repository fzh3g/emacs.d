(use-package dracula-theme
  :init
  (add-hook 'after-init-hook
            '(lambda ()
               (load-theme 'dracula t))))

;(use-package color-theme-sanityinc-tomorrow
;  :init
;  (defun light ()
;    "Activate a light color theme."
;    (interactive)
;    (color-theme-sanityinc-tomorrow-day))
;
;  (defun dark ()
;    "Activate a dark color theme."
;    (interactive)
;    (color-theme-sanityinc-tomorrow-night))
;
;  (add-hook 'after-init-hook 'color-theme-sanityinc-tomorrow-night))

(provide 'init-theme)
