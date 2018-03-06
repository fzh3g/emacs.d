;;; init-windows.el --- Emacs configuration for windows management
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

;; Some configuration for winner and windows splitting.

;;; Code:

(defun fx/split-window-vertically ()
  "Split window vertically and show other buffer in the new window."
  (interactive)
  (split-window-vertically)
  (set-window-buffer (next-window) (other-buffer)))

(defun fx/split-window-horizontally ()
  "Split window horizontally and show other buffer in the new window."
  (interactive)
  (split-window-horizontally)
  (set-window-buffer (next-window) (other-buffer)))

(global-set-key "\C-x2" #'fx/split-window-vertically)
(global-set-key "\C-x3" #'fx/split-window-horizontally)

(defun fx/split-window-horizontally-instead ()
  "Rearrange split windows horizontally."
  (interactive)
  (save-excursion
    (delete-other-windows)
    (fx/split-window-horizontally)))

(defun fx/split-window-vertically-instead ()
  "Rearrange split windows vertically."
  (interactive)
  (save-excursion
    (delete-other-windows)
    (fx/split-window-vertically)))

(global-set-key "\C-x|" #'fx/split-window-horizontally-instead)
(global-set-key "\C-x_" #'fx/split-window-vertically-instead)

(use-package fullframe
  :defer t
  :init
  (progn
    (eval-after-load 'ibuffer (fullframe ibuffer ibuffer-quit))
    (eval-after-load 'package (fullframe list-packages quit-window))
    ))

(use-package winum
  :config
  (progn
    ;; Spaceline
    (setq winum-auto-setup-mode-line nil)
    (setq winum-ignored-buffers '(" *which-key*"))
    (set-face-attribute 'winum-face nil :weight 'bold)
    (define-key winum-keymap (kbd "M-0") 'winum-select-window-0-or-10)
    (define-key winum-keymap (kbd "M-1") 'winum-select-window-1)
    (define-key winum-keymap (kbd "M-2") 'winum-select-window-2)
    (define-key winum-keymap (kbd "M-3") 'winum-select-window-3)
    (define-key winum-keymap (kbd "M-4") 'winum-select-window-4)
    (define-key winum-keymap (kbd "M-5") 'winum-select-window-5)
    (define-key winum-keymap (kbd "M-6") 'winum-select-window-6)
    (define-key winum-keymap (kbd "M-7") 'winum-select-window-7)
    (define-key winum-keymap (kbd "M-8") 'winum-select-window-8)
    (define-key winum-keymap (kbd "M-9") 'winum-select-window-9)
    (winum-mode)))

(use-package buffer-move
  :defer t
  :init (setq-default buffer-move-behavior 'move)
  :bind
  (("C-x w <up>" . buf-move-up)
   ("C-x w <down>" . buf-move-down)
   ("C-x w <left>" . buf-move-left)
   ("C-x w <right>". buf-move-right)))

(use-package winner
  :defer t
  :init
  (progn
    (setq winner-boring-buffers '("*Completions*"
                                  "*Compile-Log*"
                                  "*inferior-lisp*"
                                  "*Apropos*"
                                  "*Help*"
                                  "*cvs*"
                                  "*Buffer List*"
                                  "*Ibuffer*"
                                  "*esh command on file*"
                                  "*Flycheck error messages*"
                                  ))
    (winner-mode 1)
    (global-set-key (kbd "C-x 4 u") 'winner-undo)
    (global-set-key (kbd "C-x 4 r") 'winner-redo)))

(provide 'init-windows)
;;; init-windows.el ends here
