;;; init-ivy.el --- Emacs configuration for Ivy, Swiper, Counsel
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

;; Some configuration for ivy, swiper, counsel.

;;; Code:

(use-package ivy
  :diminish ivy-mode
  :config
  (progn
    (require 'counsel)
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-re-builders-alist
          '((read-file-name-internal . ivy--regex-fuzzy)
            (t . ivy--regex-plus)))
    (setq ivy-count-format "(%d/%d) ")
    (setq ivy-display-style 'fancy)
    (bind-keys
     :map ivy-minibuffer-map
     ("C-t" . ivy-toggle-fuzzy))
    (global-unset-key "\C-s"))
  :bind (("C-c C-r" . ivy-resume)
         ("C-x C-f" . counsel-find-file)
         ("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-s s"  . counsel-grep-or-swiper)
         ("C-s a" . counsel-ag)
         ("C-s u" . counsel-unicode-char)
         ("C-s l" . counsel-locate)
         ("C-s g" . counsel-git)
         ("C-s k" . counsel-git-grep)
         ("C-h v" . counsel-describe-variable)
         ("C-h S" . counsel-info-lookup-symbol)
         ("C-h f" . counsel-describe-function)
         ("C-h k" . counsel-descbinds)
         ("C-h b" . counsel-bookmark)))

(provide 'init-ivy)
;;; init-ivy.el ends here
