;;; init-markdown.el --- Emacs configuration for Markdown
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

;; Some configuration for markdown-mode.

;;; Code:

(use-package markdown-mode
  :mode "\\.\\(m[k]d\\|markdown\\)\\'"
  :defer t
  :init
  (progn
    (defun markdown-imenu-create-index ()
      (let* ((root '(nil . nil))
             cur-alist
             (cur-level 0)
             (pattern "^\\(\\(#+\\)[ \t]*\\(.+\\)\\|\\([^# \t\n=-].*\\)\n===+\\|\\([^# \t\n=-].*\\)\n---+\\)$")
             (empty-heading "-")
             (self-heading ".")
             hashes pos level heading)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward pattern (point-max) t)
            (cond
             ((setq hashes (match-string-no-properties 2))
              (setq heading (match-string-no-properties 3)
                    pos (match-beginning 1)
                    level (length hashes)))
             ((setq heading (match-string-no-properties 4))
              (setq pos (match-beginning 4)
                    level 1))
             ((setq heading (match-string-no-properties 5))
              (setq pos (match-beginning 5)
                    level 2)))
            (let ((alist (list (cons heading pos))))
              (cond
               ((= cur-level level)		; new sibling
                (setcdr cur-alist alist)
                (setq cur-alist alist))
               ((< cur-level level)		; first child
                (dotimes (i (- level cur-level 1))
                  (setq alist (list (cons empty-heading alist))))
                (if cur-alist
                    (let* ((parent (car cur-alist))
                           (self-pos (cdr parent)))
                      (setcdr parent (cons (cons self-heading self-pos) alist)))
                  (setcdr root alist))		; primogenitor
                (setq cur-alist alist)
                (setq cur-level level))
               (t				; new sibling of an ancestor
                (let ((sibling-alist (last (cdr root))))
                  (dotimes (i (1- level))
                    (setq sibling-alist (last (cdar sibling-alist))))
                  (setcdr sibling-alist alist)
                  (setq cur-alist alist))
                (setq cur-level level)))))
          (cdr root))))

    (add-hook 'markdown-mode-hook
              #'(lambda ()
                  (setq imenu-create-index-function 'markdown-imenu-create-index))))
  :config
  (progn
    (when (fboundp 'sp-local-pair)
      (sp-local-pair 'markdown-mode "`" nil :actions '(:rem autoskip))
      (sp-local-pair 'markdown-mode "'" nil :actions nil))
    ))

(use-package mmm-mode
  :commands mmm-parse-buffer
  :config
  (progn
    ;; http://jblevins.org/log/mmm
    (defun my-mmm-markdown-auto-class (lang &optional submode)
      "Define a mmm-mode class for LANG in `markdown-mode' using SUBMODE.
If SUBMODE is not provided, use `LANG-mode' by default."
      (let ((class (intern (concat "markdown-" lang)))
            (submode (or submode (intern (concat lang "-mode"))))
            (front (concat "^```" lang "[\n\r]+"))
            (back "^```"))
        (mmm-add-classes (list (list class :submode submode :front front :back back)))
        (mmm-add-mode-ext-class 'markdown-mode nil class)))

    ;; Mode names that derive directly from the language name
    (mapc 'my-mmm-markdown-auto-class
          '("awk" "bibtex" "c" "cpp" "java" "css" "latex" "html" "lisp" "makefile"
            "markdown" "python" "r"))

    ;; Mode names that differ from the language name
    (my-mmm-markdown-auto-class "fortran" 'f90-mode)
    (my-mmm-markdown-auto-class "elisp" 'emacs-lisp-mode)
    (my-mmm-markdown-auto-class "shell" 'shell-script-mode)))

(provide 'init-markdown)
;;; init-markdown.el ends here
