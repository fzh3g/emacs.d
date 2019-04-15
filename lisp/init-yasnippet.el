;;; init-yasnippet.el --- Emacs configuration for Yasnippet
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

;; Some configuration for yasnippet.

;;; Code:

(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-global-mode yas-minor-mode)
  :init
  (progn
    ;; disable yas minor mode map
    ;; use hippie-expand instead
    (setq yas-minor-mode-map (make-sparse-keymap))

    (setq yas-triggers-in-field t)
    ;; on multiple keys, fall back to completing read
    ;; typically this means helm
    (setq yas-prompt-functions '(yas-completing-prompt))

    (defun fx/load-yasnippet ()
      (unless yas-global-mode
        (progn
          (yas-global-mode 1)
          (setq yas-wrap-around-region t)))
      (yas-minor-mode 1))

    (dolist (hook '(prog-mode-hook
                    markdown-mode-hook
                    org-mode-hook))
      (add-hook hook 'fx/load-yasnippet))

    (defun fx/yasnippet-off ()
      (yas-minor-mode -1)
      (setq yas-dont-activate-functions t))

    (dolist (hook '(term-mode-hook
                    shell-mode-hook
                    eshell-mode-hook))
      (add-hook hook 'fx/yasnippet-off)))
  :config
  (progn
    (add-hook 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
    (define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
    ;;  We need to know whether the smartparens was enabled, see
    ;; `yas-before-expand-snippet-hook' below.
    (defvar smartparens-enabled-initially t
      "Stored whether smartparens is originally enabled or not.")

    (add-hook 'yas-before-expand-snippet-hook
              (lambda ()
                ;; If enabled, smartparens will mess snippets expanded
                ;; by `hippie-expand`
                (setq smartparens-enabled-initially smartparens-mode)
                (smartparens-mode -1)))
    (add-hook 'yas-after-exit-snippet-hook
              (lambda ()
                (when smartparens-enabled-initially
                  (smartparens-mode 1))))))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
