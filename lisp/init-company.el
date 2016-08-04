;;; init-company.el --- Emacs configuration for company-mode
;;
;; Copyright (c) 2015-2016 Faxiang Zheng
;;
;; Author: Faxiang Zheng <fxzheng0906@outlook.com>
;; URL: https://github.com/zhengfaxiang/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Some configuration for automatic completion using company-mode.

;;; Code:

(defun fx//show-snippets-in-company (backend)
  (if (and (listp backend) (member 'company-yasnippet backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(use-package company
  :diminish company-mode
  :defer t
  :init
  (progn
    (setq company-idle-delay 0.2
          company-minimum-prefix-length 2
          company-require-match nil
          company-dabbrev-ignore-case nil
          company-dabbrev-downcase nil
          company-show-numbers t
          company-auto-complete nil
          company-tooltip-align-annotations t)
    (add-hook 'after-init-hook 'global-company-mode))
  :config
  (progn
    (setq company-backends '(company-capf
                             company-files
                             (company-dabbrev-code
                              company-etags
                              company-keywords)
                             company-dabbrev))
    (define-key company-active-map (kbd "C-h") 'company-abort)
    (setq company-backends (mapcar 'fx//show-snippets-in-company
                                   company-backends))
    (defun fx/toggle-shell-auto-completion-based-on-path ()
      "Suppress automatic completion on remote paths."
      (if (file-remote-p default-directory)
          (setq-local company-idle-delay nil)
        (setq-local company-idle-delay 0.2)))
    (add-hook 'eshell-directory-change-hook
              'fx/toggle-shell-auto-completion-based-on-path)))

(use-package company-quickhelp
  :if (display-graphic-p)
  :defer t
  :init
  (progn
    (add-hook 'company-mode-hook 'company-quickhelp-mode)
    (with-eval-after-load 'company
      (setq company-frontends
            (delq 'company-echo-metadata-frontend company-frontends)))))

(use-package company-statistics
  :defer t
  :init
  (progn
    (setq company-statistics-file (concat fx-cache-directory
                                          "company-statistics-cache.el"))
    (add-hook 'company-mode-hook 'company-statistics-mode)))

(defun fx/company-for-tex ()
  (make-variable-buffer-local 'company-backends)
  (use-package company-auctex
    :defer t
    :init
    (progn
      (company-auctex-init)
      (setq company-backends (mapcar 'fx//show-snippets-in-company
                                     company-backends)))))

(defun fx/company-for-python ()
  (make-variable-buffer-local 'company-backends)
  (use-package company-jedi
    :defer t
    :init
    (progn
      (add-to-list 'company-backends
                   '(company-jedi :with company-yasnippet)))))

(defun fx/company-for-c-c++ ()
  (make-variable-buffer-local 'company-backends)
  (use-package company-irony
    :defer t
    :init
    (progn
      (add-to-list 'company-backends
                   '(company-irony :with company-yasnippet))))
  (use-package company-c-headers
    :defer t
    :init
    (progn
      (add-to-list 'company-backends
                   '(company-c-headers :with company-yasnippet)))))

(defun fx/company-for-css ()
  (make-variable-buffer-local 'company-backends)
  (add-to-list 'company-backends
               '(company-css :with company-yasnippet)))

(defun fx/company-for-web ()
  (make-variable-buffer-local 'company-backends)
  (use-package company-web
    :defer t
    :init
    (progn
      (add-to-list 'company-backends
                   '(company-css :with company-yasnippet))
      (add-to-list 'company-backends
                   '(company-web-html :with company-yasnippet)))))

(add-hook 'css-mode-hook #'fx/company-for-css)

(add-hook 'web-mode-hook #'fx/company-for-web)

(dolist (hook '(LaTeX-mode-hook TeX-mode-hook))
  (add-hook hook 'fx/company-for-tex))

(add-hook 'python-mode-hook 'fx/company-for-python)

(dolist (hook '(c-mode-hook c++-mode-hook objc-mode-hook))
  (add-hook hook 'fx/company-for-c-c++))

(provide 'init-company)
;;; init-company.el ends here
