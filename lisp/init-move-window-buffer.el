(use-package window-numbering
  :config
  (progn
    (window-numbering-mode 1)
    ;; (custom-set-faces '(window-numbering-face
    ;;                     ((t (:foreground "#ff1493" :weight bold)))))
    ))

(use-package buffer-move
  :defer t
  :init (setq-default buffer-move-behavior 'move)
  :bind
  (("C-x C-m <up>" . buf-move-up)
   ("C-x C-m <down>" . buf-move-down)
   ("C-x C-m <left>" . buf-move-left)
   ("C-x C-m <right>". buf-move-right)))

(provide 'init-move-window-buffer)
