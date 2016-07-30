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

(global-unset-key (kbd "C-s"))
(global-set-key (kbd "C-s s") 'isearch-forward)

(use-package ivy
  :diminish ivy-mode
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-re-builders-alist
          '((read-file-name-internal . ivy--regex-fuzzy)
            (t . ivy--regex-plus)))
    (setq ivy-count-format "(%d/%d) ")
    (setq ivy-display-style 'fancy)
    (bind-keys
     :map ivy-minibuffer-map
     ("C-t" . ivy-toggle-fuzzy)))
  :bind (("C-c C-r" . ivy-resume)))

(use-package swiper
  :bind (("C-s SPC"  . counsel-grep-or-swiper)))

(use-package counsel
  :bind (("C-x C-f" . counsel-find-file)
         ("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-s a" . counsel-ag)
         ("C-s u" . counsel-unicode-char)
         ("C-s l" . counsel-locate)
         ("C-s j" . counsel-git)
         ("C-s k" . counsel-git-grep)
         ("C-h v" . counsel-describe-variable)
         ("C-h S" . counsel-info-lookup-symbol)
         ("C-h f" . counsel-describe-function)
         ("C-h k" . counsel-descbinds)
         ("C-h b" . counsel-bookmark)))

(provide 'init-ivy)
;;; init-ivy.el ends here
