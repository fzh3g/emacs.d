;;; init-git.el --- Emacs configuration for Git
;; -*- coding: utf-8 -*-
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

;; Some configuration for diff-hl and magit.

;;; Code:

(use-package git-gutter+
  :diminish git-gutter+-mode
  :commands global-git-gutter+-mode
  :init
  (progn
    (global-git-gutter+-mode t)
    (setq git-gutter+-hide-gutter t
          git-gutter+-modified-sign " "
          git-gutter+-diff-option "-w"
          git-gutter+-verbosity 0)
    (add-hook 'magit-pre-refresh-hook 'git-gutter+-refresh))
  :config
  (progn
    (require 'git-gutter-fringe+)
    (define-fringe-bitmap 'git-gutter-fr+-added
      [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224
           224 224 224 224 224 224 224 224]
      nil nil 'center)
    (define-fringe-bitmap 'git-gutter-fr+-modified
      [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224
           224 224 224 224 224 224 224 224]
      nil nil 'center)
    (define-fringe-bitmap 'git-gutter-fr+-deleted
      [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
      nil nil 'center)))

(use-package magit
  :commands (magit-status
             magit-blame-mode
             magit-log
             magit-commit)
  :init
  (progn
    (setq magit-completing-read-function 'ivy-completing-read
          magit-save-some-buffers nil
          magit-process-popup-time -1)

    (when *win32*
      (setenv "GIT_ASKPASS" "git-gui--askpass"))

    (global-set-key (kbd "M-<f12>") 'magit-status)
    (global-set-key (kbd "C-x g") 'magit-status))
  :config
  (progn
    ;; Don't let magit-status mess up window configurations
    ;; http://whattheemacsd.com/setup-magit.el-01.html
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))

    (defun magit-quit-session ()
      "Restores the previous window configuration and kills the magit buffer"
      (interactive)
      (kill-buffer)
      (jump-to-register :magit-fullscreen))

    (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

    (when *is-a-mac*
      (eval-after-load 'magit
        (add-hook 'magit-mode-hook (lambda () (local-unset-key (kbd "M-h"))))))))

(provide 'init-git)
;;; init-git.el ends here
