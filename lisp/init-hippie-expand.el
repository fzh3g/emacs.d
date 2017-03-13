;;; init-hippie-expand.el --- Emacs configuration for hippie-expand
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

;; Some configuration for hippie-expand.

;;; Code:

(use-package hippie-exp
  :bind ("M-/" . hippie-expand)
  :config
  (progn
    (setq
     hippie-expand-try-functions-list
     '(try-expand-dabbrev
       try-expand-dabbrev-all-buffers
       try-expand-dabbrev-from-kill
       try-complete-file-name-partially
       try-complete-file-name
       try-expand-all-abbrevs
       try-expand-list
       try-expand-line
       try-complete-lisp-symbol-partially
       try-complete-lisp-symbol))))

(provide 'init-hippie-expand)
;;; init-hippie-expand.el ends here
