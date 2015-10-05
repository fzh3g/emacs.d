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
          org-completion-use-ido t
          org-edit-timestamp-down-means-later t
          org-fast-tag-selection-single-key 'expert
          org-tags-column 80
          org-src-fontify-natively t)

    (setq org-agenda-include-diary t
          org-agenda-start-day nil
          org-agenda-window-setup 'curent-window
          org-agenda-inhibit-startup t
          org-agenda-use-tag-inheritance nil)

    (setq org-clock-persistence-insinuate t
          org-clock-persist t
          org-clock-in-resume t
          org-clock-in-switch-to-state "STARTED"
          org-clock-into-drawer t
          org-clock-out-remove-zero-time-clocks t)

    (setq org-default-notes-file "~/.notes.org")
    
    (setq org-directory "~/org/")
    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
             "* TODO %?\n %i\n %a")
            ("b" "Bug" entry (file+headline "~/org/bug.org" "Bugs")
             "* %?\nEntered on %U\n %i\n %a")
            ("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
             "* %?\nEntered on %U\n %i\n %a")
            ("j" "Journal" entry (file+datetree "~/org/journal.org" "Journal")
             "* %?\nEntered on %U\n %i\n %a")))

    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "|" "DONE(d)")
                  (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
                  (sequence "|" "CANCELLED(c)"))))
    
    (bind-key "C-c c" 'org-capture)
    (bind-key "C-c b" 'org-iswitchb)
    (bind-key "C-c a" 'org-agenda)
    (bind-key "C-c l" 'org-store-link))
  :config
  (progn
    ;; http://wenshanren.org/?p=327
    (defun org-insert-src-block (src-code-type)
      "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
      (interactive
       (let ((src-code-types
              '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
                "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
                "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
                "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
                "scheme" "sqlite")))
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

    (setq truncate-lines nil
          word-wrap t)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((R . t)
       (C . t)
       (ditaa . t)
       (dot . t)
       (emacs-lisp . t)
       (gnuplot . t)
       (haskell . nil)
       (latex . t)
       (ledger . t)
       (ocaml . nil)
       (octave . t)
       (python . t)
       (ruby . t)
       (screen . nil)
       (sh . t)
       (sql . nil)
       (sqlite . t)))))

(provide 'init-org)
;;; init-org.el ends here
