;;; init-fullframe.el --- Emacs configuration for fullframe
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

;; Some configuration for fullframe.

;;; Code:

(use-package fullframe
  :defer t
  :init
  (progn
    (eval-after-load 'ibuffer (fullframe ibuffer ibuffer-quit))
    (eval-after-load 'magit (fullframe magit-status magit-mode-quit-window))
    (eval-after-load 'magit (fullframe projectile-vc magit-mode-quit-window))
    (eval-after-load 'package (fullframe list-packages quit-window))
    ))

(provide 'init-fullframe)
;;; init-fullframe.el ends here
