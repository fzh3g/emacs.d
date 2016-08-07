;;; init-org.el --- Emacs configuration for Org
;; -*- coding: utf-8 -*-
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

;; Some configuration for org-mode.

;;; Code:

(use-package gnuplot
  :defer t
  :mode "\\.gp$")

(use-package org
  :mode ("\\.\\(org\\|org_archive\\)$" . org-mode)
  :defer t
  :init
  (progn
    (which-key-declare-prefixes "C-c c" "org")
    (setq org-clock-persist-file
          (concat fx-cache-directory "org-clock-save.el")
          org-idl-locations-file
          (concat fx-cache-directory "org-id-locations")
          org-log-done t
          org-startup-with-inline-image t
          org-startup-indented t
          org-hide-leading-stars t
          org-edit-timestamp-down-means-later t
          org-fast-tag-selection-single-key 'expert
          org-src-fontify-natively t
          org-cycle-separator-lines 0
          org-completion-use-ido t
          org-ellipsis "⤵")

    (setq org-clock-persist t
          org-clock-in-resume t
          org-clock-in-switch-to-state "STARTED"
          org-clock-into-drawer t
          org-clock-out-remove-zero-time-clocks t)
    )
  :bind (("C-c c c" . org-capture)
         ("C-c c l" . org-store-link)
         ("C-c c o" . org-clock-out)
         ("C-c c t" . org-todo-list)
         )
  :config
  (progn
    (setq org-default-notes-file "notes.org")

    (setq org-capture-templates
          '(("t" "Todo" entry (file "")
             "* TODO %?\n%i\n %a" :clock-resume t)
            ("n" "Note" entry (file "")
             "* %? :NOTE:\n%U\n%i\n %a" :clock-resume t)))

    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d@/!)")
                  (sequence "WAIT(w@/!)" "CANCELLED(c@/!)"))))

    (setq org-time-clocksum-format
          '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

    (require 'org-indent)

    ;; markdown export
    (require 'ox-md)
    ;; org-latex
    (require 'ox-latex)
    (setq org-latex-default-packages-alist
          (delete '("" "fixltx2e" nil) org-latex-default-packages-alist))
    (add-to-list 'org-latex-packages-alist '("" "minted"))
    (setq org-latex-listings 'minted)
    (setq org-latex-minted-options
          '(;; ("frame" "leftline")
            ;; ("bgcolor" "lightgray")
            ("framesep" "2mm")
            ("fontsize" "\\footnotesize")
            ("mathescape" "")
            ("linenos" "")
            ))
    (setq org-latex-pdf-process
          '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
    ;; http://wenshanren.org/?p=327
    (defun fx/org-insert-src-block (src-code-type)
      "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
      (interactive
       (let ((src-code-types
              '("asymptote" "awk" "calc" "C" "css" "C++" "ditaa"
                "emacs-lisp" "fortran" "gnuplot" "idl" "java"
                "js" "latex" "lisp" "matlab" "octave" "perl" "plantuml"
                "python" "R" "ruby" "sass" "screen" "sh" "scheme"
                "sql" "sqlite")))
         (list (completing-read "Source code type: " src-code-types))))
      (progn
        (newline-and-indent)
        (insert (format "#+BEGIN_SRC %s\n" src-code-type))
        (newline-and-indent)
        (insert "#+END_SRC\n")
        (previous-line 2)
        (org-edit-src-code)))

    (add-hook 'org-mode-hook
              #'(lambda ()
                  (local-set-key (kbd "C-c s") 'fx/org-insert-src-block)
                  ))

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((R . t)
       (ruby . t)
       (python . t)
       (matlab . t)
       (octave . t)
       (dot . t)
       (ditaa . t)
       (emacs-lisp . t)
       (gnuplot . t)
       (js . t)
       (plantuml . t)
       (perl . t)
       (sh . t)
       (sql . t)
       (sqlite . t)))

    ;; edit src blocks with specific modes
    (add-to-list 'org-src-lang-modes '("idl" . idlwave))
    (add-to-list 'org-src-lang-modes '("fortran" . f90))))

(use-package org-agenda
  :init (setq org-agenda-restore-windows-after-quit t)
  :bind (("C-c c o" . org-agenda)
         ("C-c c a" . org-agenda-list)))

(use-package org-pomodoro
  :init
  (progn
    (setq org-pomodoro-play-sounds 1)
    (when *is-a-mac*
      (setq org-pomodoro-audio-player "/usr/bin/afplay"))
    )
  :bind ("C-c c p" . org-pomodoro))

(use-package org-bullets
  :defer t
  :init (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package toc-org
  :defer t
  :init
  (progn
    (setq toc-org-max-depth 10)
    (add-hook 'org-mode-hook 'toc-org-enable)))

(use-package htmlize
  :defer t)

(provide 'init-org)
;;; init-org.el ends here
