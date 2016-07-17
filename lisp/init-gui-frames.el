;;; init-gui-frames.el --- Emacs configuration for graphics display
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

;; Some configuration for graphics display.

;;; Code:

;;----------------------------------------------------------------------
;; Supress GUI features
;;----------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

(setq initial-major-mode 'org-mode)
(setq initial-scratch-message
      (concat "\n"
              "#          ::::::::::   :::   :::       :::      ::::::::   ::::::::\n"
              "#         :+:         :+:+: :+:+:    :+: :+:   :+:    :+: :+:    :+:\n"
              "#        +:+        +:+ +:+:+ +:+  +:+   +:+  +:+        +:+\n"
              "#       +#++:++#   +#+  +:+  +#+ +#++:++#++: +#+        +#++:++#++\n"
              "#      +#+        +#+       +#+ +#+     +#+ +#+               +#+\n"
              "#     #+#        #+#       #+# #+#     #+# #+#    #+# #+#    #+#\n"
              "#    ########## ###       ### ###     ###  ########   ########\n\n\n"))

;; http://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
(setq ad-redefinition-action 'accept)

;; pretty symbols
(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode))

;; Show a marker in the left fringe for lines not in the buffer
(setq-default indicate-empty-lines t)
(define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
(setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)
(set-fringe-bitmap-face 'tilde 'font-lock-type-face)

;; Window size and features
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))
(menu-bar-mode -1)

;; adjust opacity
(defun adjust-opacity (frame incr)
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(global-set-key (kbd "<f11>") 'toggle-frame-fullscreen)
(global-set-key (kbd "M-<f11>") 'toggle-frame-maximized)
(global-set-key (kbd "M-C-8") (lambda ()
                                (interactive)
                                (adjust-opacity nil -5)))
(global-set-key (kbd "M-C-9") (lambda ()
                                (interactive)
                                (adjust-opacity nil 5)))
(global-set-key (kbd "M-C-0") (lambda ()
                                (interactive)
                                (modify-frame-parameters nil `((alpha . 100)))))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (unless window-system
                (set-frame-parameter nil 'menu-bar-lines 0)))))

;; Toggle line highlighting in all buffers
(global-hl-line-mode t)

;; Show paren mode
;; (show-paren-mode 1)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("Emacs  "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; time management
;; (setq display-time-24hr-format t)
(setq display-time-default-load-average nil)
;; (setq display-time-day-and-date t)
(display-time)

(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
