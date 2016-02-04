(use-package helm
  :init
  (progn
    (require 'helm-config)
    (setq-default helm-split-window-in-side-p t
                  helm-prevent-escaping-from-minibuffer t
                  helm-bookmark-show-location t
                  helm-always-two-windows t
                  helm-display-header-line nil
                  helm-move-to-line-cycle-in-source t
                  helm-ff-search-library-in-sexp t
                  helm-candidate-number-limit 500
                  helm-ff-file-name-history-use-recentf t)

    ;; fuzzy matching setting
    (setq-default helm-M-x-fuzzy-match t
                  helm-buffers-fuzzy-matching t
                  helm-recentf-fuzzy-match t
                  helm-semantic-fuzzy-match t
                  helm-imenu-fuzzy-match t
                  helm-locate-fuzzy-match t
                  helm-apropos-fuzzy-match t
                  helm-ag-fuzzy-match t
                  helm-lisp-fuzzy-completion t)

    (when (executable-find "curl")
      (setq-default helm-google-suggest-use-curl-p t))

    ;; shell history.
    (add-hook 'shell-mode-hook
              '(lambda ()
                 (local-set-key (kbd "C-c C-l") 'helm-comint-input-ring)))
    (add-hook 'inferior-python-mode-hook
              '(lambda ()
                 (local-set-key (kbd "C-c C-l") 'helm-comint-input-ring)))
    (add-hook 'eshell-mode-hook
              '(lambda ()
                 (local-set-key (kbd "C-c C-l") 'helm-eshell-history)))
    (add-hook 'idlwave-shell-mode-hook
              '(lambda ()
                 (local-set-key (kbd "C-c C-l") 'helm-comint-input-ring)))

    ;(setq-default helm-autoresize-max-height 50)
    (helm-mode 1)
    (helm-autoresize-mode t))
  :bind
  (("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-x b" . helm-mini)
   ("C-h o" . helm-occur)
   ("C-h i" . helm-imenu)
   ("C-h <SPC>" . helm-all-mark-rings)
   ("C-c h x" . helm-register)
   ("C-c h M-:" . helm-eval-expression-with-eldoc)
   ("C-x C-o" . helm-find-files)
   ("C-c h g" . helm-do-grep)
   ("C-h a" . helm-apropos)
   ("C-h r" . helm-mark-ring)
   ("C-c h C-c w" . helm-wikipedia-suggest)))

(use-package helm-ag
  :defer t
  :init
  (progn
    (setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
    (setq helm-ag-insert-at-point 'symbol))
  :bind
  (("C-M-g" . helm-do-ag)))

(use-package helm-swoop
  :defer t
  :init
  (progn
    (setq helm-multi-swoop-edit-save t
          helm-swoop-split-with-multiple-windows nil
          helm-swoop-split-direction 'split-window-vertically
          helm-swoop-move-to-line-cycle t
          helm-swoop-speed-or-color t))
  :config
  (progn
    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
    (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop))
  :bind
  (("M-i" . helm-swoop)
   ("M-I" . helm-swoop-back-to-last-point)
   ("C-c M-i" . helm-multi-swoop)
   ("C-x M-i" . helm-multi-swoop-all)))

(use-package helm-c-yasnippet
  :defer t
  :init
  (progn
    (setq helm-yas-space-match-any-greedy t)
    (global-set-key (kbd "C-c y") 'helm-yas-complete)))

(provide 'init-helm)

