;;; init-lua-mode.el --- Emacs configuration for Lua
;; -*- coding: utf-8 -*-
;;
;; Copyright (c) 2015-2021 Faxiang Zheng
;;
;; Author: Faxiang Zheng <fxzheng0906@outlook.com>
;; URL: https://github.com/zhengfaxiang/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Some configuration for lua-mode.

;;; Code:

(use-package lua-mode
  :defer t
  :mode "\\.lua\\'"
  :interpreter "lua"
  :config
  (progn
    (setq lua-indent-level 4
          lua-indent-string-contents t)
    (setq-local imenu-generic-expression
              '(("Variable" "^ *\\([a-zA-Z0-9_.]+\\) *= *{ *[^ ]*$" 1)
                ("Function" "function +\\([^ (]+\\).*$" 1)
                ("Module" "^ *module +\\([^ ]+\\) *$" 1)
                ("Variable" "^ *local +\\([^ ]+\\).*$" 1)))))

(provide 'init-lua-mode)
;;; init-lua-mode.el ends here
