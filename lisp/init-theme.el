;;; init-theme.el --- Emacs configuration for color theme
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

;; Some configuration for color theme.

;;; Code:

;; (use-package monokai-theme
;;   :config
;;   (load-theme 'monokai t))

(use-package leuven-theme
  :config
  (load-theme 'leuven t))

(provide 'init-theme)
;;; init-theme.el ends here
