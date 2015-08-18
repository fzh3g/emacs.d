;; flycheck
(use-package flycheck
  :defer t
  :diminish flycheck-mode
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode)
  :config
  (use-package flycheck-pos-tip
    :defer t
    :init
    (progn
      (defun my-flycheck-pos-tip-error-messages (errors)
        (when (not (company-search-mode))
          (flycheck-pos-tip-error-messages errors)))
      (setq flycheck-display-errors-function 'my-flycheck-pos-tip-error-messages))))

;; auto-fill-mode
(add-hook 'change-log-mode-hook 'turn-on-auto-fill)

(defun my:local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

;; whitespace
(add-hook 'prog-mode-hook (lambda ()
			    (interactive)
			    (setq show-trailing-whitespace 1)))

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

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
  :defer t
  :diminish projectile-mode
  :init
  (progn
    (setq projectile-enable-caching t)
    (setq projectile-cache-file (concat fx-cache-directory
                                        "projectile.cache"))
    (setq projectile-known-projects-file (concat fx-cache-directory
                                                 "projectile-bookmarks.eld"))
    (setq projectile-completion-system 'helm)
    (setq projectile-indexing-method 'alien)
    (add-hook 'prog-mode-hook
              '(lambda ()
                 (projectile-mode)))))

(provide 'init-programming)
