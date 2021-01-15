;;; init-theme.el --- Emacs configuration for color theme
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

;; Some configuration for color theme.

;;; Code:

(use-package leuven-theme
  :config
  ;; load color theme
  (load-theme 'leuven t)
  ;; refresh powerline
  (when (fboundp 'powerline-reset)
    (powerline-reset)))

(provide 'init-theme)
;;; init-theme.el ends here
