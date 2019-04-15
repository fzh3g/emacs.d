;;; init-exec-path.el --- Emacs configuration for path
;; -*- coding: utf-8 -*-
;;
;; Copyright (c) 2015-2019 Faxiang Zheng
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

(when (and window-system
           (memq window-system '(mac ns x)))
  (use-package exec-path-from-shell
    :config
    (progn
      (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"
                     "LANG" "LC_CTYPE" "GOPATH" "IDL_LIB_DIR" "IDL_PATH"
                     "IDL_STARTUP"))
        (add-to-list 'exec-path-from-shell-variables var))
      (setq-default exec-path-from-shell-arguments nil)
      (exec-path-from-shell-initialize))))

(provide 'init-exec-path)
;;; init-exec-path.el ends here
