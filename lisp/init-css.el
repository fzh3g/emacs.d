;;; init-css-mode.el --- Emacs configuration for css
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

;; Some configuration for css-mode.

;;; Code:

(use-package css-mode
  :defer t
  :config
  (progn
    (add-hook 'css-mode-hook
              #'(lambda ()
                  (setq imenu-generic-expression
                        '((nil "^\\([^\s-].*+\\(?:,\n.*\\)*\\)\\s-{$" 1)))))))

(provide 'init-css)
;;; init-css.el ends here
