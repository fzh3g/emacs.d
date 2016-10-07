;;; init-ivy.el --- Emacs configuration for Ivy, Swiper, Counsel
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

;; Some configuration for ivy, swiper, counsel.

;;; Code:

(global-unset-key (kbd "C-s"))

(use-package isearch
  :bind (("C-s s" . isearch-forward)
         ("C-r" . isearch-backward)))

(use-package ivy
  :diminish ivy-mode
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t
          ivy-count-format "(%d/%d) "
          ivy-initial-inputs-alist nil
          ivy-display-style 'fancy)

    ;; fuzzy matching
    (require 'flx)
    (setq ivy-re-builders-alist
          '((swiper . ivy--regex-plus)
            (t . ivy--regex-fuzzy)))
    (bind-keys
     :map ivy-minibuffer-map
     ("C-t" . ivy-toggle-fuzzy)
     ("C-j" . ivy-immediate-done)
     ("<return>" . ivy-alt-done)))
  :bind (("C-s r" . ivy-resume)))

(use-package swiper
  :bind (("C-s SPC"  . swiper)))

(use-package counsel
  :bind (("C-x C-f" . counsel-find-file)
         ("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-s a" . counsel-ag)
         ("C-s u" . counsel-unicode-char)
         ("C-s l" . counsel-locate)
         ("C-s j" . counsel-git)
         ("C-s k" . counsel-git-grep)
         ("C-s t" . counsel-load-theme)
         ("C-h v" . counsel-describe-variable)
         ("C-h S" . counsel-info-lookup-symbol)
         ("C-h f" . counsel-describe-function)
         ("C-h k" . counsel-descbinds)
         ("C-h b" . counsel-bookmark))
  :config
  (progn
    ;; shell history.
    (add-hook 'shell-mode-hook
              (lambda ()
                (define-key shell-mode-map (kbd "C-c C-l")
                  'counsel-shell-history)))
    (add-hook 'inferior-python-mode-hook
              (lambda ()
                (define-key inferior-python-mode-map (kbd "C-c C-l")
                  'counsel-shell-history)))
    (add-hook 'idlwave-shell-mode-hook
              (lambda ()
                (define-key idlwave-shell-mode-map (kbd "C-c C-l")
                  'counsel-shell-history)))
    (add-hook 'eshell-mode-hook
              (lambda ()
                (define-key eshell-mode-map (kbd "C-c C-l")
                  'counsel-esh-history)))))

(provide 'init-ivy)
;;; init-ivy.el ends here
