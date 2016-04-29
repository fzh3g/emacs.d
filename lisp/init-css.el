(use-package scss-mode
  :defer t
  :init
  (progn
    (add-hook 'scss-mode-hook 'flycheck-mode)
    (add-hook 'scss-mode-hook 'rainbow-delimiters-mode))
  :mode ("\\.scss\\'" . scss-mode))

(use-package less-css-mode
  :defer t
  :init
  (add-hook 'less-css-mode-hook 'rainbow-delimiters-mode)
  :mode ("\\.less\\'" . less-css-mode))

(use-package sass-mode
  :defer t
  :init
  (add-hook 'sass-mode-hook 'flycheck-mode)
  :mode ("\\.sass\\'" . sass-mode))

(provide 'init-css)
