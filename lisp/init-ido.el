(use-package ido
  :init
  (progn
    (ido-mode 1)
    (setq ido-save-directory-list-file (concat fx-cache-directory "ido.last"))
    (setq ido-use-faces nil
          ido-use-filename-at-point 'guess
          ido-auto-merge-work-directories-length 0
          ido-use-virtual-buffers t
          ido-default-buffer-method 'selected-window)))

(use-package flx-ido
  :init
  (progn
    (setq ido-enable-flex-matching t)
    (flx-ido-mode 1)))

(use-package ido-vertical-mode
  :init
  (progn
    (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
    (ido-vertical-mode 1)))

(use-package ido-ubiquitous
  :init
  (defvar ido-default-item nil)
  (ido-ubiquitous-mode 1))

(use-package ido-hacks
  :defer t)

(use-package idomenu
  :bind ("C-h i" . helm-imenu))

(provide 'init-ido)
