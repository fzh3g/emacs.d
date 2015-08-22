(use-package yaml-mode
  :mode "\\.yml$"
  :config
  (progn
    (define-key yaml-mode-map "\C-m" 'newline-and-indent)
    ))

(provide 'init-yaml)
