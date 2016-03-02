(use-package color-theme-sanityinc-tomorrow
  :init
  (add-hook 'after-init-hook
            '(lambda ()
               (load-theme 'sanityinc-tomorrow-day t)))
  :config
  (progn
    (defun light-theme ()
      "Load a light color theme."
      (interactive)
      (load-theme 'sanityinc-tomorrow-day t)
      (when (fboundp 'powerline-reset)
        (powerline-reset)))
    (defun dark-theme ()
      "Load a dark color theme."
      (interactive)
      (load-theme 'sanityinc-tomorrow-night t)
      (when (fboundp 'powerline-reset)
        (powerline-reset)))))

(provide 'init-theme)
