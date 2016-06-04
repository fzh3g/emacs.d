;; Emoji support :cat: :thumbsup:
(use-package emoji-cheat-sheet-plus
  :commands (emoji-cheat-sheet-plus-insert
             emoji-cheat-sheet-plus-buffer
             emoji-cheat-sheet-plus-display-mode)
  :init
  (progn
    (defun fx//delay-emoji-cheat-sheet-hook ()
      (run-at-time 0.1 nil 'emoji-cheat-sheet-plus-display-mode))
    (dolist (hook '(prog-mode-hook
                    markdown-mode-hook
                    org-mode-hook))
      (add-hook hook 'fx//delay-emoji-cheat-sheet-hook))
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
