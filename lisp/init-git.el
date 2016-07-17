;;; init-git.el --- Emacs configuration for Git
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

(use-package diff-hl
  :init
  (progn
    (setq diff-hl-side 'right)
    (global-diff-hl-mode)
    (unless (display-graphic-p)
      (setq diff-hl-side 'left)
      (diff-hl-margin-mode))
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

(use-package magit
  :commands (magit-status
             magit-blame-mode
             magit-log
             magit-commit)
  :init
  (progn
    (setq magit-completing-read-function 'magit-builtin-completing-read
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
