(require 'helm-gtags)

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-fuzzy-match t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-suggested-key-mapping t
 helm-gtags-prefix-key "\C-cg")

(dolist (hook '(c-mode-hook
                c++-mode-hook
                java-mode-hook
                python-mode-hook
                f90-mode-hook
                fortran-mode-hook
                ))
  (add-hook hook
            (lambda ()
              (helm-gtags-mode 1))))

(add-hook 'helm-gtags-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-j") 'helm-gtags-select)
             (local-set-key (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
             (local-set-key (kbd "C-c g t") 'helm-gtags-find-tag)
             (local-set-key (kbd "C-c g r") 'helm-gtags-find-rtag)
             (local-set-key (kbd "C-c g s") 'helm-gtags-find-symbol)
             (local-set-key (kbd "C-c g d") 'helm-gtags-dwim)
             (local-set-key (kbd "C-c g t") 'helm-gtags-pop-stack)))

(provide 'init-gtags)