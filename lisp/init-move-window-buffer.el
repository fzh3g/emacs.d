;;; init-move-window-buffer.el --- Emacs configuration for window number
;; and buffer moving
;;
;; Copyright (c) 2015-2016 Faxiang Zheng
;;
;; Author: Faxiang Zheng <fxzheng0906@outlook.com>
;; URL: https://github.com/zhengfaxiang/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Some configuration for window-numbering and buffer move.

;;; Code:

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
  (("C-x C-m <up>" . buf-move-up)
   ("C-x C-m <down>" . buf-move-down)
   ("C-x C-m <left>" . buf-move-left)
   ("C-x C-m <right>". buf-move-right)))

(provide 'init-move-window-buffer)
;;; init-move-window-buffer.el ends here
