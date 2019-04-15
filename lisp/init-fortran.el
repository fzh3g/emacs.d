;;; init-fortran.el --- Emacs configuration for fortran
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

;; Some configuration for fortran-mode and f90-mode.

;;; Code:

(use-package fortran-mode
  :defer t
  :mode "\\.\\(f\\|F\\)$"
  :init
  (add-hook 'fortran-mode-hook
            #'(lambda ()
                (flycheck-mode -1))))

(use-package f90-mode
  :defer t
  :mode "\\.\\(f90\\|F90\\|f95\\|F95\\|g90\\|g95\\)$"
  :init (add-hook 'f90-mode-hook
                  #'(lambda ()
                      (abbrev-mode 1)
                      (flycheck-mode -1))))

(provide 'init-fortran)
;;; init-fortran.el ends here
