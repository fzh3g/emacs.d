;;; init-programming.el --- Emacs configuration for programming
;; -*- coding: utf-8 -*-
;;
;; Copyright (c) 2015-2019 Faxiang Zheng
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
  :diminish flycheck-mode
  :defer t
  :init
  (progn
    (setq flycheck-indication-mode 'right-fringe
          flycheck-check-syntax-automatically '(save mode-enabled))
    (add-hook 'prog-mode-hook 'flycheck-mode)
    (add-hook 'c++-mode-hook
              #'(lambda ()
                  (setq flycheck-gcc-language-standard "c++14"
                        flycheck-clang-language-standard "c++14"))))
  :config
  (progn
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [0 0 0 0 0 4 12 28 60 124 252 124 60 28 12 4 0 0 0 0])
    (setq flycheck-mode-line-prefix "ε")
    (define-key flycheck-mode-map flycheck-keymap-prefix nil)
    (setq flycheck-keymap-prefix (kbd "C-c e"))
    (define-key flycheck-mode-map flycheck-keymap-prefix
      flycheck-command-map)))

;; auto-fill-mode
(add-hook 'change-log-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook
          #'(lambda ()
              (set (make-local-variable 'comment-auto-fill-only-comments) t)))

;; Highlight and allow to open http link at point in programming buffers
;; goto-address-prog-mode only highlights links in strings and comments
(add-hook 'prog-mode-hook 'goto-address-prog-mode)
;; Highlight and follow bug references in comments and strings
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)

;; subword
(use-package subword
  :diminish subword-mode
  :defer t
  :init
  (progn
    (unless (category-docstring ?U)
      (define-category ?U "Uppercase")
      (define-category ?u "Lowercase"))
    (modify-category-entry (cons ?A ?Z) ?U)
    (modify-category-entry (cons ?a ?z) ?u)
    (add-hook 'prog-mode-hook #'subword-mode)))

;; eldoc
(use-package eldoc
  :diminish eldoc-mode
  :defer t
  :config
  (dolist (hook '(emacs-lisp-mode-hook
                  lisp-interaction-mode-hook
                  eval-expression-minibuffer-setup-hook
                  irony-mode-hook))
    (add-hook hook #'eldoc-mode)))

;; folding
(use-package hideshow
  :diminish hs-minor-mode
  :defer t
  :init
  (progn
    (dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
      (add-hook hook 'hs-minor-mode)))
  :config
  (progn
    (bind-keys :map hs-minor-mode-map
               ("C-c z c" . hs-hide-block)
               ("C-c z o" . hs-show-block)
               ("C-c z C" . hs-hide-all)
               ("C-c z O" . hs-show-all)
               ("C-c z l" . hs-hide-level)
               ("C-c z z" . hs-toggle-hiding)
               ("C-c z <tab>" . hs-toggle-hiding))))

;; projectile
(use-package projectile
  :diminish projectile-mode
  :bind (("C-x p p" . projectile-switch-project)
         ("C-x p f" . projectile-find-file)
         ("C-x p D" . projectile-dired)
         ("C-x p e" . projectile-recentf)
         ("C-x p b" . projectile-switch-to-buffer))
  :init
  (progn
    (setq projectile-keymap-prefix (kbd "C-x p"))
    (setq projectile-enable-caching nil)
    (setq projectile-sort-order 'recentf)
    (setq projectile-cache-file (concat fx-cache-directory
                                        "projectile.cache"))
    (setq projectile-known-projects-file (concat fx-cache-directory
                                                 "projectile-bookmarks.eld"))
    (setq projectile-completion-system 'ivy)
    (setq projectile-indexing-method 'alien))
  :config
  (progn
    (projectile-mode)
    (projectile-cleanup-known-projects)))

(use-package imenu
  :defer t
  :config
  (setq imenu-auto-rescan t))

(use-package imenu-anywhere
  :bind ("C-c i i" . ivy-imenu-anywhere))

(provide 'init-programming)
;;; init-programming.el ends here
