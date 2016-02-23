(use-package org
  :mode ("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode)
  :defer t
  :init
  (progn
    (setq org-clock-persist-file
          (concat fx-cache-directory "org-clock-save.el")
          org-log-done t
          org-startup-with-inline-image t
          org-startup-indented t
          org-hide-leading-stars t
          org-completion-use-ido t
          org-edit-timestamp-down-means-later t
          org-fast-tag-selection-single-key 'expert
          org-tags-column 80
          org-src-fontify-natively t)

    (setq org-agenda-include-diary nil
          org-agenda-compact-blocks t
          org-agenda-sticky t
          org-agenda-start-day nil
          org-agenda-window-setup 'curent-window
          org-agenda-inhibit-startup t
          org-agenda-use-tag-inheritance nil)

    (setq org-refile-targets '((nil :maxlevel . 5)
                               (org-agenda-files :maxlevel . 5))
          org-refile-use-cache nil
          org-refile-use-outline-path t
          org-outline-path-complete-in-steps nil)

    (setq org-clock-persistence-insinuate t
          org-clock-persist t
          org-clock-in-resume t
          ;org-clock-in-switch-to-state "STARTED"
          org-clock-into-drawer t
          org-clock-out-remove-zero-time-clocks t)
    (setq org-time-clocksum-format
          '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

    (setq org-default-notes-file "~/org/capture.org")

    (setq org-capture-templates
          '(("t" "Todo" entry (file "")
             "* TODO %?\n%i\n %a" :clock-resume t)
            ("n" "Note" entry (file "")
             "* %? :NOTE:\n%U\n%i\n %a" :clock-resume t)))

    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "DONE(d@/!)")
                  (sequence "WAIT(w@/!)" "CANCELLED(c@/!)"))))

    (bind-key "C-c c" 'org-capture)
    (bind-key "C-c b" 'org-iswitchb)
    (bind-key "C-c a" 'org-agenda)
    (bind-key "C-c l" 'org-store-link))
  :config
  (progn
    ;;http://stackoverflow.com/questions/22988092/emacs-org-mode-export-markdown
    (eval-after-load "org"
      '(require 'ox-md nil t))
    ;; org-latex
    (require 'ox-latex)
    (add-to-list 'org-latex-packages-alist '("" "minted"))
    (setq org-latex-listings 'minted)
    (setq org-latex-minted-options
          '(("frame" "leftline")
            ("bgcolor" "lightgray")
            ("framesep" "2mm")
            ("fontsize" "\\footnotesize")
            ("mathescape" "")
            ("linenos" "")))
    (setq org-latex-pdf-process
          '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
    ;; http://wenshanren.org/?p=327
    (defun org-insert-src-block (src-code-type)
      "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
      (interactive
       (let ((src-code-types
              '("asymptote" "awk" "calc" "C" "clojure" "css" "C++" "ditaa"
                "emacs-lisp" "fortran" "gnuplot" "haskell" "idl" "java"
                "js" "latex" "ledger" "lisp" "mathematica" "matlab"
                "octave" "ocaml" "perl" "plantuml" "python" "R" "ruby"
                "sass" "scala" "sh" "scheme" "sql" "sqlite")))
         (list (completing-read "Source code type: " src-code-types))))
      (progn
        (newline-and-indent)
        (insert (format "#+BEGIN_SRC %s\n" src-code-type))
        (newline-and-indent)
        (insert "#+END_SRC\n")
        (previous-line 2)
        (org-edit-src-code)))

    (bind-key "C-c s e" 'org-edit-src-code)
    (bind-key "C-c s i" 'org-insert-src-block)

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((R . t)
       (ruby . t)
       (python . t)
       (matlab . t)
       (mathematica . t)
       (maxima . t)
       (octave . t)
       (dot . t)
       (ditaa . t)
       (emacs-lisp . t)
       (gnuplot . t)
       (plantuml . t)
       (sh . t)
       (haskell . nil)
       (latex . t)
       (ledger . nil)
       (ocaml . nil)
       (screen . nil)
       (sql . nil)
       (sqlite . nil)))))

(use-package org-bullets
  :defer t
  :init (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package htmlize
  :defer t)

(provide 'init-org)
;;; init-org.el ends here
