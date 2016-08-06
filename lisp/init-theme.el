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

;; ;; @see https://plus.google.com/106672400078851000780/posts/KhTgscKE8PM
;; (defadvice load-theme (before disable-themes-first activate)
;;   "Diable all themes before load a new one."
;;   (dolist (i custom-enabled-themes)
;;     (disable-theme i)))

(use-package monokai-theme
  :config
  (load-theme 'monokai t)
  (when (fboundp 'powerline-reset)
    (powerline-reset)))

(provide 'init-theme)
;;; init-theme.el ends here
