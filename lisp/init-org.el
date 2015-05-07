(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(global-set-key "\C-cC" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq-default org-directory "~/org/")
(setq-default org-default-notes-file "~/.notes.org")
(setq-default org-capture-templates
              '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
                 "* TODO %?\n %i\n %a")
                ("b" "Bug" entry (file+headline "~/org/bug.org" "Bugs")
                 "* %?\nEntered on %U\n %i\n %a")
                ("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
                 "* %?\nEntered on %U\n %i\n %a")
                ("j" "Journal" entry (file+datetree "~/org/journal.org" "Journal")
                 "* %?\nEntered on %U\n %i\n %a")))

(setq-default org-startup-indented t
              org-log-done 'time
              org-log-done 'note
              org-completion-use-ido t
              org-edit-timestamp-down-means-later t
              org-agenda-start-on-weekday nil
              org-agenda-span 14
              org-agenda-include-diary t
              org-agenda-window-setup 'curent-window
              org-fast-tag-selection-single-key 'expert
              org-tags-column 80
              org-agenda-inhibit-startup t
              org-agenda-use-tag-inheritance nil)


; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq-default org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
; Targets start with the file name - allows creating level 1 tasks
(setq-default org-refile-use-outline-path (quote file))
; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq-default org-outline-path-complete-in-steps t)


(setq-default org-todo-keywords
              (quote ((sequence "TODO(t)" "|" "DONE(d)")
                      (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
                      (sequence "|" "CANCELLED(c)"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq-default org-clock-persistence-insinuate t)
(setq-default org-clock-persist t)
(setq-default org-clock-in-resume t)

;; Change task state to STARTED when clocking in
(setq-default org-clock-in-switch-to-state "STARTED")
;; Save clock data and notes in the LOGBOOK drawer
(setq-default org-clock-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq-default org-clock-out-remove-zero-time-clocks t)

;; Show the clocked-in task - if any - in the header line
(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)


(eval-after-load 'org
   '(progn
      (require 'org-clock)
      ; @see http://irreal.org/blog/?p=671
      (setq-default org-src-fontify-natively t)
      (defun soft-wrap-lines ()
        "Make lines wrap at window edge and on word boundary,
        in current buffer."
        (interactive)
        (setq-default truncate-lines nil)
        (setq-default word-wrap t)
        )
      (add-hook 'org-mode-hook '(lambda ()
                                  (setq-default evil-auto-indent nil)
                                  (soft-wrap-lines)
                                  ))))

(eval-after-load 'org
  '(progn
     (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
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
