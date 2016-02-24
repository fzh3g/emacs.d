(use-package matlab-load
  :defer t
  :init
  (progn
    (defun fx/init-matlab ()
      (make-variable-buffer-local 'company-backends)
      (add-to-list 'company-backends
                   '(company-matlab-shell :with company-yasnippet)))
    (dolist (hook '(matlab-mode-hook matlab-shell-mode-hook))
      (add-hook hook 'fx/init-matlab))))

(provide 'init-matlab)
