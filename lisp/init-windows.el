;;; init-windows.el --- Emacs configuration for windows management
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

;; Some configuration for winner and windows splitting.

;;; Code:

;; winner
(use-package winner
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

;; When splitting window, show (other-buffer) in the new window
(defun fx/split-window-vertically ()
  (interactive)
  (split-window-vertically)
  (set-window-buffer (next-window) (other-buffer)))

(defun fx/split-window-horizontally ()
  (interactive)
  (split-window-horizontally)
  (set-window-buffer (next-window) (other-buffer)))

(global-set-key "\C-x2" #'fx/split-window-vertically)
(global-set-key "\C-x3" #'fx/split-window-horizontally)

;; Rearrange split windows
(defun fx/split-window-horizontally-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (fx/split-window-horizontally)))

(defun fx/split-window-vertically-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (fx/split-window-vertically)))

(global-set-key "\C-x|" #'fx/split-window-horizontally-instead)
(global-set-key "\C-x_" #'fx/split-window-vertically-instead)

(provide 'init-windows)
;;; init-windows.el ends here
