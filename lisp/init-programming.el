;; flycheck
(require 'flycheck)

(if (fboundp 'global-flycheck-mode)
    (global-flycheck-mode +1)
  (add-hook 'prog-mode-hook 'flycheck-mode))


(setq flycheck-display-errors-function
      #'flycheck-display-error-messages-unless-error-list)

;; auto-fill-mode
(add-hook 'change-log-mode-hook 'turn-on-auto-fill)
(global-set-key (kbd "C-c q") 'auto-fill-mode)

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
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'hs-minor-mode-hook
          '(lambda ()
             (local-set-key (kbd "M-<left>") 'hs-hide-block)
             (local-set-key (kbd "M-<right>") 'hs-show-block)
             (local-set-key (kbd "M-<up>") 'hs-hide-all)
             (local-set-key (kbd "M-<down>") 'hs-show-all)
             (local-set-key (kbd "C-c @ l") 'hs-hide-level)
             (local-set-key (kbd "C-c @ c") 'hs-toggle-hiding)))

;; projectile
(require 'projectile)
(require 'helm-projectile)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'helm)
(setq projectile-indexing-method 'alien)
(add-hook 'prog-mode-hook
          '(lambda ()
             (projectile-mode)
             (helm-projectile-on)))

;; speedbar
(require 'sr-speedbar)
(setq speedbar-show-unknown-files t
      speedbar-use-images nil
      sr-speedbar-width 27
      sr-speedbar-default-width 27
      sr-speedbar-max-width 36
      sr-speedbar-right-side nil)
(global-set-key (kbd "<f6>") 'sr-speedbar-toggle)

;;iedit-mode
;(global-set-key (kbd "C-c ; i") 'iedit-mode-toggle-on-function)

(provide 'init-programming)