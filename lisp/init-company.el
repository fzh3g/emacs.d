(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(setq company-require-match nil)
(setq company-auto-complete nil)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)
(setq company-dabbrev-downcase nil)
(setq company-show-numbers t)
(setq company-transformaers 'company-sort-by-occurence)
(setq company-tooltip-align-annotations t)
(setq company-begin-commands '(self-insert-command))

(add-hook 'company-mode-hook
	  '(lambda ()
	    (add-to-list 'company-backends 'company-math-symbols-unicode)))

(defun my:company-for-latex ()
  (require 'company-auctex)
  (company-auctex-init)
  (require 'company-math)
  (add-to-list 'company-backends 'company-math-symbols-latex))

(defun my:company-for-c ()
  (add-to-list 'company-backends 'company-cmake)
  (add-to-list 'company-backends 'company-irony)
  (add-to-list 'company-backends 'company-c-headers))

(defun my:company-for-python ()
  (setq company-backends (delete 'company-ropemacs company-backends)))

(add-hook 'TeX-mode-hook 'my:company-for-latex)
(add-hook 'c-mode-hook 'my:company-for-c)
(add-hook 'c++-mode-hook 'my:company-for-c)
(add-hook 'python-mode-hook 'my:company-for-python)


(provide 'init-company)
