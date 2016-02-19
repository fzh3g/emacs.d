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
