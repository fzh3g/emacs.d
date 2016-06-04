;; Emoji support :cat: :thumbsup:
(use-package emoji-cheat-sheet-plus
  :commands (emoji-cheat-sheet-plus-insert
             emoji-cheat-sheet-plus-buffer
             emoji-cheat-sheet-plus-display-mode)
  :init
  (progn
    (dolist (hook '(prog-mode-hook
                    markdown-mode-hook
                    org-mode-hook))
      (add-hook hook 'emoji-cheat-sheet-plus-display-mode))
    (global-set-key (kbd "C-c e i") 'emoji-cheat-sheet-plus-insert)
    (global-set-key (kbd "C-c e b") 'emoji-cheat-sheet-plus-buffer)))

(use-package company-emoji
  :defer t
  :init
  (progn
    (with-eval-after-load 'company
      (add-to-list 'company-backends '(company-emoji
                                       :with company-yasnippet)))
    (setq company-emoji-insert-unicode nil)))

(provide 'init-emoji)
;;; init-emoji.el ends here
