(require 'yasnippet)

(yas-global-mode 1)

(setq yas-wrap-around-region t)

(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)

(setq-default mode-require-final-newline nil)
(provide 'init-yasnippet)
