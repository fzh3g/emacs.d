(defun fx//show-snippets-in-company (backend)
  (if (and (listp backend) (member 'company-yasnippet backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

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
                  company-tooltip-align-annotations t)

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
  :config
  (progn
    (define-key company-active-map (kbd "C-h") 'company-abort)
    (setq company-backends (mapcar 'fx//show-snippets-in-company
                                   company-backends))
    (defun fx/toggle-shell-auto-completion-based-on-path ()
      "Suppress automatic completion on remote paths."
      (if (file-remote-p default-directory)
          (setq-local company-idle-delay nil)
        (setq-local company-idle-delay 0.2)))
    (add-hook 'eshell-directory-change-hook
              'fx/toggle-shell-auto-completion-based-on-path)))

(use-package company-statistics
  :defer t
  :init
  (progn
    (setq-default company-statistics-file (concat fx-cache-directory
                                                  "company-statistics-cache.el"))
    (add-hook 'company-mode-hook 'company-statistics-mode)))

(defun fx/company-for-tex ()
  (make-variable-buffer-local 'company-backends)
  (use-package company-auctex
    :defer t
    :init
    (progn
      (company-auctex-init)
      (setq company-backends (mapcar 'fx//show-snippets-in-company
                                     company-backends)))))

(defun fx/company-for-python ()
  (make-variable-buffer-local 'company-backends)
  (use-package company-jedi
   :defer t
   :init
   (progn
     (add-to-list 'company-backends
                  '(company-jedi :with company-yasnippet)))))

(defun fx/company-for-c-c++ ()
  (make-variable-buffer-local 'company-backends)
  (setq company-backends (delete
                          '(company-semantic :with company-yasnippet)
                          company-backends))
  (use-package company-irony
    :defer t
    :init
    (progn
      (add-to-list 'company-backends
                   '(company-irony :with company-yasnippet))))
  (use-package company-c-headers
    :defer t
    :init
    (progn
      (add-to-list 'company-backends
                   '(company-c-headers :with company-yasnippet)))))

(dolist (hook '(LaTeX-mode-hook TeX-mode-hook))
  (add-hook hook 'fx/company-for-tex))

(add-hook 'python-mode-hook 'fx/company-for-python)

(dolist (hook '(c-mode-hook c++-mode-hook objc-mode-hook))
  (add-hook hook 'fx/company-for-c-c++))

(provide 'init-company)
