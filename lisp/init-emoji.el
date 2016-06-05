;; Emoji support 🐱 👍
(use-package emojify
  :defer t
  :init
  (progn
    (add-hook 'after-init-hook 'global-emojify-mode)))

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
