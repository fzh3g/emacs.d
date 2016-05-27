;; whitespace
(use-package whitespace
  :defer t
  :init
  (progn
    (setq whitespace-style '(face
                             trailing
                             tabs
                             tab-mark
                             empty
                             spaces
                             space-mark
                             newline
                             newline-mark
                             indentation::space))
    (global-set-key (kbd "C-c w") 'whitespace-mode)
    (add-hook 'prog-mode-hook
              '(lambda ()
                 (setq show-trailing-whitespace t)
                 (add-hook 'before-save-hook 'whitespace-cleanup))))
  :config
  (progn
    (set-face-attribute 'whitespace-space nil
                        :background nil
                        :foreground (face-attribute
                                     'font-lock-warning-face
                                     :foreground))
    (set-face-attribute 'whitespace-tab nil
                        :background nil)
    (set-face-attribute 'whitespace-indentation nil
                        :background nil)))

(provide 'init-whitespace)
;;; init-whitespace.el ends here
