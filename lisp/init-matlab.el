;;; init-matlab.el --- Emacs configuration for Matlab
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

;; Some configuration for matlab-mode.

;;; Code:

(use-package matlab-load
  :defer t
  :init
  (progn
    (defun fx/init-matlab ()
      (make-variable-buffer-local 'company-backends)
      (add-to-list 'company-backends
                   '(company-matlab-shell :with company-yasnippet))
      (define-key matlab-mode-map (kbd "M-s") nil))
    (dolist (hook '(matlab-mode-hook matlab-shell-mode-hook))
      (add-hook hook 'fx/init-matlab))))

(provide 'init-matlab)
;;; init-matlab.el ends here
