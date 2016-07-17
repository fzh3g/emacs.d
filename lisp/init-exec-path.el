;;; init-exec-path.el --- Emacs configuration for path
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

;; Some configuration for getting path from shell.

;;; Code:

(use-package exec-path-from-shell
  :init (when (memq window-system '(mac ns))
          (exec-path-from-shell-initialize))
  :config
  (progn
    (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"
                   "LANG" "LC_CTYPE"))
      (add-to-list 'exec-path-from-shell-variables var))))

(provide 'init-exec-path)
;;; init-exec-path.el ends here
