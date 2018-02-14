;;; init-whitespace.el --- Emacs configuration for whitespace
;; -*- coding: utf-8 -*-
;;
;; Copyright (c) 2015-2018 Faxiang Zheng
;;
;; Author: Faxiang Zheng <fxzheng0906@outlook.com>
;; URL: https://github.com/zhengfaxiang/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Some configuration for whitespace.

;;; Code:

;; whitespace
(use-package whitespace
  :defer t
  :init
  (progn
    (setq whitespace-style '(face
                             trailing
                             tabs
                             tab-mark
                             empty
                             spaces
                             space-mark
                             newline
                             newline-mark
                             indentation::space))
    (global-set-key (kbd "C-c w") 'whitespace-mode)
    (add-hook 'prog-mode-hook
              #'(lambda ()
                  ;; (set-face-attribute 'trailing-whitespace nil
                  ;;                     :background
                  ;;                     (face-attribute 'font-lock-comment-face
                  ;;                                     :foreground))
                  (setq show-trailing-whitespace t)
                  ;; (add-hook 'before-save-hook 'whitespace-cleanup)
                  ))
    (global-set-key (kbd "C-x f w") 'whitespace-cleanup))
  :config
  (progn
    (set-face-attribute 'whitespace-space nil
                        :background nil
                        :foreground (face-attribute
                                     'font-lock-warning-face
                                     :foreground))
    (set-face-attribute 'whitespace-tab nil
                        :background nil)
    (set-face-attribute 'whitespace-indentation nil
                        :background nil)))

(provide 'init-whitespace)
;;; init-whitespace.el ends here
