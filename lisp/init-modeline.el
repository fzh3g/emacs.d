;;; init-modeline.el --- Emacs configuration for modeline
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

;; Some configuration for modeline with powerline.

;;; Code:

;; powerline
(use-package powerline
  :config
  (progn
    (setq powerline-default-separator 'wave)
    (powerline-default-theme)
    ))

;; clean mode-line
(defvar mode-line-cleaner-alist
  `(;; Minor modes
    (company-mode . "")
    (yas-minor-mode . "")
    (hs-minor-mode . "")
    (eldoc-mode . "")
    (abbrev-mode . "")
    (undo-tree-mode . "")
    (highlight-symbol-mode . "")
    (helm-mode . "")
    (smartparens-mode . "")
    (hi-lock-mode . "")
    (which-key-mode . "")
    (page-break-lines-mode . "")
    (irony-mode . "")
    (subword-mode . "")
    (auto-revert-mode . "")
    (org-indent-mode . "")
    (visual-line-mode . "")
    (outline-minor-mode . "")
    (reftex-mode . "")
    (google-this-mode . "")
    (hungry-delete-mode . "")
    (latex-extra-mode . "")
    (flyspell-mode . "")
    ;; Major modes
    (lisp-interaction-mode . "λ")
    (python-mode . "Py")
    (emacs-lisp-mode . "EL"))
  "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

(defun clean-mode-line ()
  "Clean minor/major mode in modeline."
  (interactive)
  (dolist (cleaner mode-line-cleaner-alist)
    (let* ((mode (car cleaner))
           (mode-str (cdr cleaner))
           (old-mode-str (cdr (assq mode minor-mode-alist))))
      (when old-mode-str
        (setcar old-mode-str mode-str))
      ;; major mode
      (when (eq mode major-mode)
        (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

(provide 'init-modeline)
;;; init-modeline.el ends here
