(defun fx/c-mode-common-setup ()
  (setq c-default-style "linux")
  (setq c-basic-offset 4)
  (setq cc-search-directories
        '("." "/usr/include" "/usr/local/include/*" "../*/include"))
  (c-set-offset 'substatement-open 0))

(add-hook 'c-mode-common-hook 'fx/c-mode-common-setup)

(use-package irony
  :defer t
  :init
  (progn
    (dolist (hook '(c-mode-hook
                    c++-mode-hook
                    objc-mode-hook))
      (add-hook hook 'irony-mode)
      (add-hook hook 'highlight-indentation-mode))
    
    ;; replace the `completion-at-point' and `complete-symbol' bindings in
    ;; irony-mode's buffers by irony-mode's function
    (defun my-irony-mode-hook ()
      (define-key irony-mode-map [remap completion-at-point]
        'irony-completion-at-point-async)
      (define-key irony-mode-map [remap complete-symbol]
        'irony-completion-at-point-async))

    (add-hook 'irony-mode-hook 'my-irony-mode-hook)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

(use-package irony-eldoc
  :defer t
  :init
  (add-hook 'irony-mode-hook 'irony-eldoc))

(use-package cmake-mode
  :defer t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))


(provide 'init-cc-mode)
