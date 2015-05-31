(require 'window-numbering)
(require 'buffer-move)
(window-numbering-mode 1)


(custom-set-faces '(window-numbering-face ((t (:foreground "#ff1493" :weight bold)))))


(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

(setq buffer-move-behavior 'move)

(provide 'init-move-window-buffer)
