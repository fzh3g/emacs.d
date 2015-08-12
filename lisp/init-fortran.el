(use-package fortran-mode
  :defer t
  :mode "\\.\\(f\\|F\\)$")

(use-package f90-mode
  :defer t
  :mode "\\.\\(f90\\|F90\\|f95\\|F95\\|g90\\|g95\\)$"
  :init
  (add-hook 'f90-mode-hook
            '(lambda ()
               (abbrev-mode 1))))

(provide 'init-fortran)
