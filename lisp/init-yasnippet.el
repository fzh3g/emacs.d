(use-package yasnippet
  :commands yas-global-mode
  :init
  (progn
    ;; disable yas minor mode map
    ;; use hippie-expand instead
    (setq yas-minor-mode-map (make-sparse-keymap))

    (setq yas-triggers-in-field t)

    (define-key yas-minor-mode-map (kbd "M-s-/") 'yas-next-field)
    
    (defun fx/load-yasnippet ()
      (unless yas-global-mode
        (progn
          (yas-global-mode 1)
          (setq yas-wrap-around-region t)))
      (yas-minor-mode 1))

    (dolist (hook '(prog-mode-hook
                    markdown-mode-hook
                    org-mode-hook))
      (add-hook hook 'fx/load-yasnippet))

    (defun fx/yasnippet-off ()
      (yas-minor-mode -1)
      (setq yas-dont-activate t))

    (dolist (hook '(term-mode-hook
                    shell-mode-hook
                    eshell-mode-hook))
      (add-hook hook 'fx/yasnippet-off)))
  :config
  (progn
    (add-hook 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
    (define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
    ;;  We need to know whether the smartparens was enabled, see
    ;; `yas-before-expand-snippet-hook' below.
    (defvar smartparens-enabled-initially t
      "Stored whether smartparens is originally enabled or not.")

    (add-hook 'yas-before-expand-snippet-hook (lambda ()
                                                ;; If enabled, smartparens will mess snippets expanded by `hippie-expand`
                                                (setq smartparens-enabled-initially smartparens-mode)
                                                (smartparens-mode -1)))
    (add-hook 'yas-after-exit-snippet-hook (lambda ()
                                             (when smartparens-enabled-initially
                                               (smartparens-mode 1))))))

(provide 'init-yasnippet)
