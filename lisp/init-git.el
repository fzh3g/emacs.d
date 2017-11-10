;;; init-git.el --- Emacs configuration for Git
;; -*- coding: utf-8 -*-
;;
;; Copyright (c) 2015-2017 Faxiang Zheng
;;
;; Author: Faxiang Zheng <fxzheng0906@outlook.com>
;; URL: https://github.com/zhengfaxiang/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Some configuration for Git.

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
  :bind (("C-x g s" . magit-status)
         ("C-x g l" . magit-log-buffer-file)
         ("C-x g S" . magit-stage-file)
         ("C-x g U" . magit-unstage-file))
  :init
  (progn
    (setq-default magit-auto-revert-mode nil)
    (setq magit-completing-read-function 'ivy-completing-read)
    (when *win32*
      (setenv "GIT_ASKPASS" "git-gui--askpass"))
    )
  :config
  (progn
    (require 'git-rebase)
    (setq magit-display-buffer-function
          'magit-display-buffer-fullframe-status-v1)
    ))

(use-package smerge-mode
  :defer t)

(provide 'init-git)
;;; init-git.el ends here
