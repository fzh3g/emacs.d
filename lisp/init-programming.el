;;; init-programming.el --- Emacs configuration for programming
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

;; Some configuration for better programming experience.

;;; Code:

;; flycheck
(use-package flycheck
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook 'flycheck-mode)
    (add-hook 'c++-mode-hook
              #'(lambda ()
                  (setq flycheck-gcc-language-standard "c++11"
                        flycheck-clang-language-standard "c++11"))))
  :config (setq flycheck-mode-line-prefix "ε"))

;; auto-fill-mode
(add-hook 'change-log-mode-hook 'turn-on-auto-fill)

(defun my:local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

(add-hook 'prog-mode-hook
          #'(lambda ()
              (subword-mode)
              (my:local-comment-auto-fill)))

;; eldoc
(dolist (hook '(emacs-lisp-mode-hook
                lisp-interaction-mode-hook
                irony-mode-hook
                python-mode-hook))
  (add-hook hook 'turn-on-eldoc-mode))

;; hs-minor-mode
(use-package hideshow
  :diminish hs-minor-mode
  :defer t
  :init
  (progn
    (dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
      (add-hook hook 'hs-minor-mode))
    (add-hook 'hs-minor-mode-hook
              #'(lambda ()
                  (local-set-key (kbd "M-<left>") 'hs-hide-block)
                  (local-set-key (kbd "M-<right>") 'hs-show-block)
                  (local-set-key (kbd "M-<up>") 'hs-hide-all)
                  (local-set-key (kbd "M-<down>") 'hs-show-all)
                  (local-set-key (kbd "C-c @ l") 'hs-hide-level)
                  (local-set-key (kbd "C-c @ c") 'hs-toggle-hiding)))))

;; projectile
(use-package projectile
  :diminish projectile-mode
  :init
  (progn
    (setq projectile-enable-caching t)
    (setq projectile-sort-order 'recentf)
    (setq projectile-cache-file (concat fx-cache-directory
                                        "projectile.cache"))
    (setq projectile-known-projects-file (concat fx-cache-directory
                                                 "projectile-bookmarks.eld"))
    (setq projectile-completion-system 'ivy)
    (setq projectile-indexing-method 'alien)
    (add-hook 'after-init-hook
              #'(lambda ()
                  (projectile-global-mode)))
    ))

(use-package imenu-anywhere
  :defer t
  :bind ("C-s i" . ivy-imenu-anywhere))

(provide 'init-programming)
;;; init-programming.el ends here
