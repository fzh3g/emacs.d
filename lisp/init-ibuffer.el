;;; init-ibuffer.el --- Emacs configuration for ibuffer
;; -*- coding: utf-8 -*-
;;
;; Copyright (c) 2015-2018 Faxiang Zheng
;;
;; Author: Faxiang Zheng <fxzheng0906@outlook.com>
;; URL: https://github.com/zhengfaxiang/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Some configuration for ibuffer.

;;; Code:

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer))
  :config
  (progn
    (setq ibuffer-show-empty-filter-groups nil)
    (setq ibuffer-filter-group-name-face 'font-lock-doc-face)
    (use-package ibuffer-projectile
      :init
      (add-hook 'ibuffer-hook
                (lambda ()
                  (ibuffer-projectile-set-filter-groups)
                  (unless (eq ibuffer-sorting-mode 'alphabetic)
                    (ibuffer-do-sort-by-alphabetic)))))))

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
