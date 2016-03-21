(use-package moe-theme
  :config
  (progn
    (setq moe-light-pure-white-background-in-terminal t)
    (moe-dark)
    (moe-theme-set-color 'blue)
    (powerline-moe-theme)))

(provide 'init-theme)
