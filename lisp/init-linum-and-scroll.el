;;; init-linum-and-scroll.el --- Emacs configuration for linum and scrolling
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

;; Some configuration for linum and scrolling.

;;; Code:

;; show column number and line number
(use-package nlinum
  :init (setq linum-delay t)
  :config
  (dolist (hook '(prog-mode-hook
                  conf-mode-hook
                  yaml-mode-hook
                  web-mode-hook
                  markdown-mode-hook
                  matlab-mode-hook
                  css-mode-hook))
    (add-hook hook 'column-number-mode)
    (add-hook hook 'line-number-mode)
    (add-hook hook 'nlinum-mode)))

;; nice scrolling
(setq scroll-margin 5
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      auto-window-vscroll nil)

;; mouse scrolling
(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))

;; Move more quickly
(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-line 5))))

(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-line -5))))

(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))

(provide 'init-linum-and-scroll)
;;; init-linum-and-scroll.el ends here
