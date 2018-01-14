;;; init-modeline.el --- Emacs configuration for modeline
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

;; Some configuration for modeline with powerline.

;;; Code:

;; Spaceline
(use-package spaceline-config
  :config
  (progn
    (setq powerline-default-separator 'nil)
    (spaceline-spacemacs-theme)
    ))

(use-package diminish
  :init
  (progn
    (eval-after-load 'org-indent '(diminish 'org-indent-mode))
    (eval-after-load "isearch" '(diminish 'isearch-mode))
    (eval-after-load "abbrev" '(diminish 'abbrev-mode))
    (eval-after-load "reftex" '(diminish 'reftex-mode))
    (eval-after-load "autorevert" '(diminish 'auto-revert-mode))
    (eval-after-load "outline" '(diminish 'outline-minor-mode))
    (eval-after-load "simple" '(diminish 'visual-line-mode " ν"))
    (eval-after-load "simple" '(diminish 'auto-fill-function " φ"))
    ))

(add-hook 'lisp-interaction-mode-hook
          #'(lambda () (setq mode-name "λ")))

(add-hook 'emacs-lisp-mode-hook
          #'(lambda () (setq mode-name "EL")))

(add-hook 'python-mode-hook
          #'(lambda () (setq mode-name "Py")))

(add-hook 'markdown-mode-hook
          #'(lambda () (setq mode-name "M↓")))

(provide 'init-modeline)
;;; init-modeline.el ends here
