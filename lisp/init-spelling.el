;;; init-spelling.el --- Emacs configuration for Spelling
;; -*- coding: utf-8 -*-
;;
;; Copyright (c) 2015-2017 Faxiang Zheng
;;
;; Author: Faxiang Zheng <fxzheng0906@outlook.com>
;; URL: https://github.com/zhengfaxiang/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Some configuration for flyspell.

;;; Code:

(use-package flyspell
  :diminish (flyspell-mode . " ξ")
  :defer t
  :init
  (progn
    ;; (dolist (hook '(org-mode-hook markdown-mode-hook))
    ;;   (add-hook hook 'flyspell-mode))
    ;; better performance
    (setq flyspell-issue-message-flag nil)
    (cond
     ((executable-find "aspell")
      (setq ispell-program-name "aspell"
            ispell-extra-args '("--sug-mode=ultra"
                                "--lang=en_US")))
     ((executable-find "hunspell")
      (setq ispell-program-name "hunspell")
      ;; just reset dictionary to the safe one "en_US" for hunspell.
      ;; if we need use different dictionary,
      ;; we specify it in command line arguments
      (setq ispell-local-dictionary "en_US")
      (setq ispell-local-dictionary-alist
            '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil
               utf-8))))
     (t (setq ispell-program-name nil))))
  :config
  (progn
    (define-key flyspell-mode-map (kbd "C-\,") nil)
    (define-key flyspell-mode-map (kbd "C-;") nil)
    (define-key flyspell-mode-map (kbd "C-M-i") nil)
    (define-key flyspell-mode-map (kbd "C-c \$") nil)))

(use-package flyspell-correct-ivy
  :init
  (progn
    (with-eval-after-load 'flyspell
      (define-key flyspell-mode-map (kbd "C-\.")
        'flyspell-correct-word-generic)
      (setq flyspell-correct-interface 'flyspell-correct-ivy))))

(provide 'init-spelling)
;;; init-spelling.el ends here
