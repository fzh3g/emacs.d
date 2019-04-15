;;; init-uniquify.el --- Emacs configuration for uniquify
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

;; Some configuration for uniquify.

;;; Code:

;; Nicer naming of buffers for files with identical names
(use-package uniquify
  :init
  (progn
    (setq uniquify-buffer-name-style 'reverse)
    (setq uniquify-separator " • ")
    (setq uniquify-after-kill-buffer-p t)
    (setq uniquify-ignore-buffers-re "^\\*")))

(provide 'init-uniquify)
;;; init-uniquify.el ends here
