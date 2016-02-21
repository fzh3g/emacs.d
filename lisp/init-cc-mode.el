(defun fx/c-mode-common-setup ()
  (setq c-default-style "linux")
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0))

(add-hook 'c-mode-common-hook 'fx/c-mode-common-setup)

(use-package irony
  :defer t
  :init
  (progn
    (dolist (hook '(c-mode-hook
                    c++-mode-hook
                    objc-mode-hook))
      (add-hook hook 'irony-mode))

    ;; replace the `completion-at-point' and `complete-symbol' bindings in
    ;; irony-mode's buffers by irony-mode's function
    (defun my-irony-mode-hook ()
      (define-key irony-mode-map [remap completion-at-point]
        'irony-completion-at-point-async)
      (define-key irony-mode-map [remap complete-symbol]
        'irony-completion-at-point-async))

    (add-hook 'irony-mode-hook 'my-irony-mode-hook)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

(defun fx/company-for-cmake ()
  (make-variable-buffer-local 'company-backends)
  (use-package cmake-mode
   :defer t
   :mode (("CMakeLists\\.txt\\'" . cmake-mode)
          ("\\.cmake\\'" . cmake-mode))
   :init (add-to-list 'company-backends
                      '(company-cmake :with company-yasnippet))))
(add-hook 'cmake-mode-hook 'fx/company-for-cmake)

(provide 'init-cc-mode)
