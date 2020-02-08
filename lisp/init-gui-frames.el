;;; init-gui-frames.el --- Emacs configuration for graphics display
;; -*- coding: utf-8 -*-
;;
;; Copyright (c) 2015-2020 Faxiang Zheng
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

;; Supress GUI features
(setq ring-bell-function 'ignore)
(setq use-file-dialog nil
      visible-bell nil
      use-dialog-box nil
      inhibit-startup-screen t)

;; https://www.masteringemacs.org/article/disabling-prompts-emacs
(eval-after-load "startup"
  '(fset 'display-startup-echo-area-message (lambda ())))

(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (unless *is-a-mac*
    (menu-bar-mode -1)))

;; Hide mouse point when typing
(add-hook 'after-init-hook (lambda () (mouse-avoidance-mode 'exile)))

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; scratch buffer mode
(setq initial-major-mode 'text-mode)

(setq initial-scratch-message
      (concat
       "\n"
       ";;         ::::::::::   :::   :::       :::      ::::::::   ::::::::\n"
       ";;        :+:         :+:+: :+:+:    :+: :+:   :+:    :+: :+:    :+:\n"
       ";;       +:+        +:+ +:+:+ +:+  +:+   +:+  +:+        +:+\n"
       ";;      +#++:++#   +#+  +:+  +#+ +#++:++#++: +#+        +#++:++#++\n"
       ";;     +#+        +#+       +#+ +#+     +#+ +#+               +#+\n"
       ";;    #+#        #+#       #+# #+#     #+# #+#    #+# #+#    #+#\n"
       ";;   ########## ###       ### ###     ###  ########   ########\n\n\n"))


;; time display
;; (setq display-time-default-load-average nil)
;; (display-time)

;; draw underline lower
(setq x-underline-at-descent-line t)

;; Toggle line highlighting in all buffers
(add-hook 'after-init-hook 'global-hl-line-mode)

;; pretty symbols
(when (fboundp 'global-prettify-symbols-mode)
  (add-hook 'after-init-hook 'global-prettify-symbols-mode))

;; don't let the cursor go into minibuffer prompt
;; Tip taken from Xah Lee: http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties
      '(read-only
        t
        point-entered
        minibuffer-avoid-prompt
        face
        minibuffer-prompt))

;; remove annoying ellipsis when printing sexp in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

;; remove prompt if the file is opened in other clients
(defun server-remove-kill-buffer-hook ()
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))
(add-hook 'server-visit-hook 'server-remove-kill-buffer-hook)

;; Show a marker in the left fringe for lines not in the buffer
(setq-default indicate-empty-lines t)
(define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
(setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)
(set-fringe-bitmap-face 'tilde 'font-lock-type-face)

;; Fringes outside of margins
(setq-default fringes-outside-margins t)

;; adjust opacity
(defun adjust-opacity (frame incr)
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(global-set-key (kbd "C-M-8") (lambda ()
                                (interactive)
                                (adjust-opacity nil -5)))
(global-set-key (kbd "C-M-9") (lambda ()
                                (interactive)
                                (adjust-opacity nil 5)))
(global-set-key (kbd "C-M-0") (lambda ()
                                (interactive)
                                (modify-frame-parameters
                                 nil
                                 `((alpha . 100)))))

;; (modify-frame-parameters nil `((alpha . 95)))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (unless window-system
                (set-frame-parameter nil 'menu-bar-lines 0)))))

(global-set-key (kbd "<f11>") 'toggle-frame-fullscreen)
(global-set-key (kbd "M-<f11>") 'toggle-frame-maximized)

;; https://gist.github.com/3402786
(defun fx/toggle-maximize-buffer ()
  "Toggle maximize buffer."
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))
(global-set-key (kbd "<f12>") #'fx/toggle-maximize-buffer)

;; from http://emacs-doctor.com/emacs-hide-mode-line.html
(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)
(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

;; hide mode-line for smoother start
(hidden-mode-line-mode t)

(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
