;;; init-cc-mode.el --- Emacs configuration for c/c++
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

;; Some configuration for cc-mode cmake and gdb.

;;; Code:

(defun fx/c-mode-common-setup ()
  (setq c-default-style "linux")
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0))

(add-hook 'c-mode-common-hook 'fx/c-mode-common-setup)

(use-package irony
  :diminish irony-mode
  :defer t
  :init
  (progn
    (setq irony-additional-clang-options '("-std=c++11"))
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

(use-package flycheck-irony
  :defer t
  :init
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

(use-package irony-eldoc
  :defer t
  :init (add-hook 'irony-mode-hook #'irony-eldoc))

(use-package cmake-mode
  :after company
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :config
  (progn
    (add-hook 'cmake-mode-hook
              (lambda ()
                (make-variable-buffer-local 'company-backends)
                (add-to-list 'company-backends
                             '(company-cmake :with company-yasnippet))))))

(use-package gdb-mi
  :defer t
  :init
  (setq
   ;; use gdb-many-windows by default when `M-x gdb'
   gdb-many-windows t
   ;; Non-nil means display source file containing the main routine at startup
   gdb-show-main t))

(provide 'init-cc-mode)
;;; init-cc-mode.el ends here
