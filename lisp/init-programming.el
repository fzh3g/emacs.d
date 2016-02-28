;; flycheck
(use-package flycheck
  :defer t
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode))

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
                             indentation::space
                             indentation::tab))
    (global-set-key (kbd "C-c w") 'whitespace-mode))
  :config
  (progn
    (set-face-attribute 'whitespace-space nil
                        :background nil)
    (set-face-attribute 'whitespace-tab nil
                        :background nil)
    (set-face-attribute 'whitespace-indentation nil
                        :background nil)
    ))

;; auto-fill-mode
(add-hook 'change-log-mode-hook 'turn-on-auto-fill)

(defun my:local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

(add-hook 'prog-mode-hook
          '(lambda ()
             (subword-mode)
             ;; eldoc, show API doc in minibuffer echo area
             (eldoc-mode)
             (my:local-comment-auto-fill)))

;; hs-minor-mode
(dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook LaTeX-mode-hook))
  (add-hook hook 'hs-minor-mode))
(add-hook 'hs-minor-mode-hook
          '(lambda ()
             (local-set-key (kbd "M-<left>") 'hs-hide-block)
             (local-set-key (kbd "M-<right>") 'hs-show-block)
             (local-set-key (kbd "M-<up>") 'hs-hide-all)
             (local-set-key (kbd "M-<down>") 'hs-show-all)
             (local-set-key (kbd "C-c @ l") 'hs-hide-level)
             (local-set-key (kbd "C-c @ c") 'hs-toggle-hiding)))

;; projectile
(use-package projectile
  :init
  (progn
    (setq projectile-enable-caching t)
    (setq projectile-sort-order 'recentf)
    (setq projectile-cache-file (concat fx-cache-directory
                                        "projectile.cache"))
    (setq projectile-known-projects-file (concat fx-cache-directory
                                                 "projectile-bookmarks.eld"))
    (setq projectile-completion-system 'helm)
    (setq projectile-indexing-method 'alien)
    (add-hook 'after-init-hook
              '(lambda ()
                 (projectile-global-mode)))))

(provide 'init-programming)
