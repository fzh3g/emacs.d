(use-package window-numbering
  :config
  (progn
    (window-numbering-mode 1)
    (custom-set-faces '(window-numbering-face
                        ((t (:foreground "#ff1493" :weight bold)))))))

(use-package buffer-move
  :defer t
  :init (setq-default buffer-move-behavior 'move)
  :bind
  (("C-S-<up>" . buf-move-up)
   ("C-S-<down>" . buf-move-down)
   ("C-S-<left>" . buf-move-left)
   ("C-S-<right>". buf-move-right)))

(provide 'init-move-window-buffer)
