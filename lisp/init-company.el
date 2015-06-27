(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(setq company-require-match nil)
(setq company-auto-complete nil)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)
(setq company-show-numbers t)
(setq company-tooltip-align-annotations t)
(setq company-begin-commands '(self-insert-command))
(setq-default company-dabbrev-downcase nil)
(setq-default company-transformaers 'company-sort-by-occurence)

(add-hook 'company-mode-hook
          '(lambda ()
             (require 'company-math)
             (add-to-list 'company-backends 'company-math-symbols-unicode)
             (require 'company-statistics)
             (company-statistics-mode)))

(defun my:company-for-latex ()
  (require 'company-auctex)
  (company-auctex-init)
  (add-to-list 'company-backends 'company-math-symbols-latex))

(defun my:company-for-c ()
  (setq company-backends (delete 'company-semantic company-backends))
  (add-to-list 'company-backends 'company-cmake)
  (add-to-list 'company-backends 'company-irony)
  (add-to-list 'company-backends 'company-c-headers))

(defun my:company-for-python ()
  (add-to-list 'company-backends 'company-anaconda))

(add-hook 'TeX-mode-hook 'my:company-for-latex)
(add-hook 'c-mode-hook 'my:company-for-c)
(add-hook 'c++-mode-hook 'my:company-for-c)
(add-hook 'cmake-mode-hook 'my:company-for-c)
(add-hook 'python-mode-hook 'my:company-for-python)

(provide 'init-company)
