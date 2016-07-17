;;; init-spelling.el --- Emacs configuration for Spelling
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

;; Some configuration for flyspell.

;;; Code:

(use-package flyspell
  :defer t
  :init
  (progn
    (dolist (hook '(text-mode-hook TeX-mode-hook org-mode-hook markdown-mode-hook))
      (add-hook hook 'flyspell-mode))
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
      ;; if we need use different dictionary, we specify it in command line arguments
      (setq ispell-local-dictionary "en_US")
      (setq ispell-local-dictionary-alist
            '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8))))
     (t (setq ispell-program-name nil)))))

(provide 'init-spelling)
;;; init-spelling.el ends here
