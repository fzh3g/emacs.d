;;; init-cc-mode.el --- Emacs configuration for c/c++
;; -*- coding: utf-8 -*-
;;
;; Copyright (c) 2015-2020 Faxiang Zheng
;;
;; Author: Faxiang Zheng <fxzheng0906@outlook.com>
;; URL: https://github.com/zhengfaxiang/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Some configuration for cc-mode cmake and gdb.

;;; Code:

(use-package cc-mode
  :defer t
  :config
  (progn
    (setq-default c-default-style "linux"
                  c-basic-offset 4)))

(use-package irony
  :diminish irony-mode
  :defer t
  :init
  (progn
    (setq irony-additional-clang-options '("-std=c++14"))
    (when (boundp 'w32-pipe-read-delay)
      (setq w32-pipe-read-delay 0))
    (when (boundp 'w32-pipe-buffer-size)
      (setq irony-server-w32-pipe-buffer-size (* 64 1024)))
    (dolist (hook '(c-mode-hook
                    c++-mode-hook
                    objc-mode-hook))
      (add-hook hook 'irony-mode))

    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

(use-package flycheck-irony
  :defer t
  :init
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

(use-package irony-eldoc
  :defer t
  :init (add-hook 'irony-mode-hook #'irony-eldoc))

(use-package cmake-mode
  :after company
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :config
  (progn
    (add-hook 'cmake-mode-hook
              (lambda ()
                (add-to-list 'company-backends
                             '(company-cmake :with company-yasnippet))))))

(use-package gdb-mi
  :defer t
  :init
  (setq
   ;; use gdb-many-windows by default when `M-x gdb'
   gdb-many-windows t
   ;; Non-nil means display source file containing the main routine at startup
   gdb-show-main t))

(provide 'init-cc-mode)
;;; init-cc-mode.el ends here
