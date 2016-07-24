;;; init-emoji.el --- Emacs configuration for emoji
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

;; Some configuration for emoji.

;;; Code:

;; Emoji support 🐱 👍
(unless *is-a-mac*
  (use-package emojify
    :defer t
    :init
    (progn
      (add-hook 'after-init-hook 'global-emojify-mode))))

(use-package company-emoji
  :defer t
  :init
  (progn
    (with-eval-after-load 'company
      (add-to-list 'company-backends '(company-emoji
                                       :with company-yasnippet)))
    (setq company-emoji-insert-unicode t)))

(provide 'init-emoji)
;;; init-emoji.el ends here
