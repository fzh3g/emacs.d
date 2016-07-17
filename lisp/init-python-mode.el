;; jedi
(use-package jedi-core
  :defer t
  :init
  (progn
    (setq jedi:complete-on-dot t)
    (setq jedi:use-shortcuts t)
    (add-hook 'python-mode-hook
              #'(lambda ()
                  (jedi:setup)))))

(use-package pyvenv
  :defer t)

(use-package python
  :defer t
  :init
  (progn
    (defun python-default ()
      (setq python-indent 4
            python-indent-offset 4
            python-indent-guess-indent-offset nil)
      (local-set-key (kbd "C-j") 'newline-and-indent))

    (defun python-setup-shell ()
      (if (executable-find "ipython")
          (setq python-shell-interpreter "ipython")
        (setq python-shell-interpreter "python")))

    (add-hook 'python-mode-hook
              #'(lambda ()
                  (python-default)
                  (python-setup-shell)))))

(provide 'init-python-mode)
