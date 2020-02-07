;;; init-ivy.el --- Emacs configuration for Ivy, Swiper, Counsel
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

;; Some configuration for ivy, swiper, counsel.

;;; Code:

(use-package ivy
  :diminish ivy-mode
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t
          ivy-count-format "(%d/%d) "
          ivy-initial-inputs-alist nil
          ivy-virtual-abbreviate 'fullpath
          ivy-display-style 'fancy)
    ;; fuzzy matching
    (require 'flx)
    (setq ivy-re-builders-alist
          '((swiper . ivy--regex-plus)
            (t . ivy--regex-ignore-order)))
    ;; wgrep
    (add-hook 'ivy-occur-grep-mode-hook
              (lambda ()
                (require 'wgrep)
                (bind-keys :map ivy-occur-grep-mode-map
                           ("C-x C-q" . ivy-wgrep-change-to-wgrep-mode))))
    (bind-keys
     :map ivy-minibuffer-map
     ("C-t" . ivy-toggle-fuzzy)
     ("C-j" . ivy-immediate-done)
     ("<return>" . ivy-alt-done)))
  :bind (("C-c i r" . ivy-resume)))

(use-package swiper
  :bind (("C-s"  . counsel-grep-or-swiper)))

(use-package counsel
  :diminish counsel-mode
  :bind (("C-x C-f" . counsel-find-file)
         ("M-y" . counsel-yank-pop)
         ("C-c i a" . counsel-rg)
         ("C-c i u" . counsel-unicode-char)
         ("C-c i l" . counsel-locate)
         ("C-c i j" . counsel-file-jump)
         ("C-c i g" . counsel-git)
         ("C-c i k" . counsel-git-grep)
         ("C-c i t" . counsel-load-theme)
         ("C-c i f" . counsel-recentf)
         ("C-c i m" . counsel-mark-ring)
         ("C-h v" . counsel-describe-variable)
         ("C-h S" . counsel-info-lookup-symbol)
         ("C-h f" . counsel-describe-function)
         ("C-h k" . counsel-descbinds)
         ("C-h b" . counsel-bookmark)
         ("C-h a" . counsel-apropos))
  :init (add-hook 'after-init-hook 'counsel-mode)
  :config
  (progn
    (setq counsel-bookmark-avoid-dired t)
    (setq counsel-git-cmd "rg --files")
    (setq counsel-rg-base-command
          "rg -i -M 120 --no-heading --line-number --color never %s .")
    (setq counsel-grep-base-command
          "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
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

(use-package smex
  :bind ([remap execute-extended-command] . smex)
  :config
  (setq-default smex-save-file
                (expand-file-name "smex-items" fx-cache-directory)))

(provide 'init-ivy)
;;; init-ivy.el ends here
