(autoload 'fortran-mode "fortran" "major mode for FORTRAN(<=77)" t)
(add-to-list 'auto-mode-alist
             '("\\.\\(f\\|F\\)$" . fortran-mode))
(autoload 'f90-mode "f90" "major mode for FORTRAN(>=90)" t)
(add-to-list 'auto-mode-alist
             '("\\.\\(f90\\|F90\\|f95\\|F95\\|g90\\|g95\\)$" . f90-mode))

(add-hook 'f90-mode-hook
          '(lambda()
             (abbrev-mode 1)))

(provide 'init-fortran)
;;; init-fortran.el ends here