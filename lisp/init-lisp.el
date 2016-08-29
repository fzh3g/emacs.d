;;; init-lisp.el --- Emacs configuration for Lisp
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

;; Some configuration for Lisp.

;;; Code:

(use-package auto-compile
  :diminish (auto-compile-mode . "")
  :defer t
  :init
  (progn
    (setq auto-compile-display-buffer nil
          auto-compile-mode-line-counter t)
    (add-hook 'emacs-lisp-mode-hook
              #'(lambda ()
                  (auto-compile-on-save-mode)))))

(provide 'init-lisp)
;;; init-lisp.el ends here