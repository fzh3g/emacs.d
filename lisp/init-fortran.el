(use-package fortran-mode
  :defer t
  :mode "\\.\\(f\\|F\\)$"
  :init
  (add-hook 'fortran-mode-hook
            '(lambda ()
               (flycheck-mode -1))))

(use-package f90-mode
  :defer t
  :mode "\\.\\(f90\\|F90\\|f95\\|F95\\|g90\\|g95\\)$"
  :init (add-hook 'f90-mode-hook
                  '(lambda ()
                     (abbrev-mode 1)
                     (flycheck-mode -1))))

(provide 'init-fortran)
