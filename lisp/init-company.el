(use-package company
  :defer t
  :init
  (progn
    (setq-default company-idle-delay 0.2
                  company-minimum-prefix-length 2
                  company-require-match nil
                  company-dabbrev-ignore-case nil
                  company-dabbrev-downcase nil
                  company-show-numbers t
                  company-auto-complete nil
                  company-frontends '(company-pseudo-tooltip-frontend)
                  company-tooltip-align-annotations t))

  (defvar-local company-fci-mode-on-p nil)
  (add-hook 'after-init-hook 'global-company-mode)
  (defun company-turn-off-fci (&rest ignore)
    (when (boundp 'fci-mode)
      (setq company-fci-mode-on-p fci-mode)
      (when fci-mode (fci-mode -1))))

  (defun company-maybe-turn-on-fci (&rest ignore)
    (when company-fci-mode-on-p (fci-mode 1)))
  ;; Nicer looking faces
  (custom-set-faces
   '(company-tooltip-common
     ((t (:inherit company-tooltip :weight bold :underline nil))))
   '(company-tooltip-common-selection
     ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))

  (add-hook 'company-completion-started-hook 'company-turn-off-fci)
  (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
  (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci))

(use-package company-statistics
  :defer t
  :init
  (progn
    (setq-default company-statistics-file (concat fx-cache-directory
                                                  "company-statistics-cache.el"))
    (add-hook 'company-mode-hook 'company-statistics-mode)))

(use-package company-quickhelp
  :if (display-graphic-p)
  :defer t
  :init (add-hook 'company-mode-hook 'company-quickhelp-mode))

(defun fx/company-for-tex ()
  (use-package company-auctex
    :defer t
    :init (company-auctex-init)))

(use-package company-jedi
  :defer t
  :init
  (eval-after-load "company"
    '(progn
       (add-to-list 'company-backends 'company-jedi))))

(defun fx/company-for-c-c++ ()
  (setq company-backends (delete 'company-semantic company-backends))
  (use-package company-irony
    :defer t
    :init (add-to-list 'company-backends 'company-irony))
  (use-package company-c-headers
    :defer t
    :init (add-to-list 'company-backends 'company-c-headers)))

(dolist (hook '(LaTeX-mode-hook TeX-mode-hook))
  (add-hook hook 'fx/company-for-tex))

(dolist (hook '(c-mode-hook c++-mode-hook objc-mode-hook))
  (add-hook hook 'fx/company-for-c-c++))

(provide 'init-company)
