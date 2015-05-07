(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(global-set-key "\C-cC" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(defvar org-directory "~/org/")
(defvar org-default-notes-file "~/.notes.org")
(defvar org-capture-templates
  '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
	 "* TODO %?\n %i\n %a")
	("b" "Bug" entry (file+headline "~/org/bug.org" "Bugs")
	 "* %?\nEntered on %U\n %i\n %a")
    ("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
     "* %?\nEntered on %U\n %i\n %a")
	("j" "Journal" entry (file+datetree "~/org/journal.org" "Journal")
	 "* %?\nEntered on %U\n %i\n %a")))

(defvar org-startup-indented t)
(defvar org-log-done 'time)
(defvar org-log-done 'note)
(defvar org-completion-use-ido t)
(defvar org-edit-timestamp-down-means-later t)
(defvar org-agenda-start-on-weekday nil)
(defvar org-agenda-span 14)
(defvar org-agenda-include-diary t)
(defvar org-agenda-window-setup 'curent-window)
(defvar org-fast-tag-selection-single-key 'expert)
(defvar org-tags-column 80)
(defvar org-agenda-inhibit-startup t)
(defvar org-agenda-use-tag-inheritance nil)


; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
(defvar org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
; Targets start with the file name - allows creating level 1 tasks
(defvar org-refile-use-outline-path (quote file))
; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(defvar org-outline-path-complete-in-steps t)


(defvar org-todo-keywords
      (quote ((sequence "TODO(t)" "|" "DONE(d)")
              (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
              (sequence "|" "CANCELLED(c)"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(defvar org-clock-persistence-insinuate t)
(defvar org-clock-persist t)
(defvar org-clock-in-resume t)

;; Change task state to STARTED when clocking in
(defvar org-clock-in-switch-to-state "STARTED")
;; Save clock data and notes in the LOGBOOK drawer
(defvar org-clock-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(defvar org-clock-out-remove-zero-time-clocks t)

;; Show the clocked-in task - if any - in the header line
(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

(eval-after-load 'org-clock
  '(progn
     (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
     (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu)))

(eval-after-load 'org
   '(progn
      (require 'org-clock)
      ; @see http://irreal.org/blog/?p=671
      (defvar org-src-fontify-natively t)
      (defun soft-wrap-lines ()
        "Make lines wrap at window edge and on word boundary,
        in current buffer."
        (interactive)
        (defvar truncate-lines nil)
        (defvar word-wrap t)
        )
      (add-hook 'org-mode-hook '(lambda ()
                                  (defvar evil-auto-indent nil)
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
