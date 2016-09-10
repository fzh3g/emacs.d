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

;; Disable VC
(setq vc-handled-backends nil)

(use-package git-gutter
  :diminish git-gutter-mode
  :commands global-git-gutter-mode
  :init
  (progn
    (global-git-gutter-mode t)
    (setq git-gutter:update-interval 2
          git-gutter:hide-gutter t
          git-gutter:ask-p nil
          git-gutter:verbosity 0
          git-gutter:handled-backends '(git hg bzr svn)))
  :config
  (progn
    (require 'git-gutter-fringe)
    (define-fringe-bitmap 'git-gutter-fr:added
      [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224
           224 224 224 224 224 224 224 224]
      nil nil 'center)
    (define-fringe-bitmap 'git-gutter-fr:modified
      [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224
           224 224 224 224 224 224 224 224]
      nil nil 'center)
    (define-fringe-bitmap 'git-gutter-fr:deleted
      [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
      nil nil 'center)

    ;; http://stackoverflow.com/questions/23344540/emacs-update-git-gutter-annotations-when-staging-or-unstaging-changes-in-magit
    (defvar my-magit-after-stage-hooks nil
      "Hooks to be run after staging one item in magit.")
    (defvar my-magit-after-unstage-hooks nil
      "Hooks to be run after unstaging one item in magit.")

    (defadvice magit-stage-item (after run-my-after-stage-hooks activate)
      "Run `my-magit-after-stage-hooks` after staging an item in magit."
      (when (called-interactively-p 'interactive)
        (run-hooks 'my-magit-after-stage-hooks)))

    (defadvice magit-unstage-item (after run-my-after-unstage-hooks activate)
      "Run `my-magit-after-unstage-hooks` after unstaging an item in magit."
      (when (called-interactively-p 'interactive)
        (run-hooks 'my-magit-after-unstage-hooks)))

    (defun my-refresh-visible-git-gutter-buffers ()
      "Refresh git-gutter-mode on all visible git-gutter-mode buffers."
      (dolist (buff (buffer-list))
        (with-current-buffer buff
          (when (and git-gutter-mode (get-buffer-window buff))
            (git-gutter-mode t)))))

    (add-hook 'my-magit-after-unstage-hooks
              'my-refresh-visible-git-gutter-buffers)
    (add-hook 'my-magit-after-stage-hooks
              'my-refresh-visible-git-gutter-buffers)
    ))

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
