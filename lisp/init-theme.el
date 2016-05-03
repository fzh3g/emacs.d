;; (use-package moe-theme
;;   :config
;;   (progn
;;     (setq moe-light-pure-white-background-in-terminal t)
;;     (moe-dark)
;;     (moe-theme-set-color 'purple)
;;     (powerline-moe-theme)))

(use-package monokai-theme
  :config
  (load-theme 'monokai t))

(provide 'init-theme)
