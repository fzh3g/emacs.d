(use-package dracula-theme
  :init
  (add-hook 'after-init-hook
            '(lambda ()
               (load-theme 'dracula t))))

(provide 'init-theme)
