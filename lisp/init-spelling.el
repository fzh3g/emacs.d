(dolist (hook '(text-mode-hook TeX-mode-hook org-mode-hook markdown-mode-hook))
  (add-hook hook 'flyspell-mode))

;; better performance
(setq-default flyspell-issue-message-flag nil)

;; if (aspell installed) { use aspell}
;; else if (hunspell installed) { use hunspell }
;; whatever spell checker I use, I always use English dictionary
;; I prefer use aspell because:
;; 1. aspell is older
;; 2. looks Kevin Atkinson still get some road map for aspell:
;; @see http://lists.gnu.org/archive/html/aspell-announce/2011-09/msg00000.html
(defun flyspell-detect-ispell-args (&optional RUN-TOGETHER)
  "if RUN-TOGETHER is true, spell check the CamelCase words"
  (let (args)
    (when ispell-program-name
      (cond
        ((string-match "aspell$" ispell-program-name)
         ;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
         (setq args (list "--sug-mode=ultra" "--lang=en_US"))
         (if RUN-TOGETHER
           (setq args (append args '("--run-together" "--run-together-limit=16" "--run-together-min=2")))))
        ((string-match "hunspell$" ispell-program-name)
         (setq args nil))))
    args
    ))

;; Aspell Setup (recommended):
;; Skipped because it's easy.
;;
;; Hunspell Setup:
;; 1. Install hunspell from http://hunspell.sourceforge.net/
;; 2. Download openoffice dictionary extension from
;; http://extensions.openoffice.org/en/project/english-dictionaries-apache-openoffice
;; 3. That is download `dict-en.oxt'. Rename that to `dict-en.zip' and unzip
;; the contents to a temporary folder.
;; 4. Copy `en_US.dic' and `en_US.aff' files from there to a folder where you
;; save dictionary files; I saved it to `~/usr_local/share/hunspell/'
;; 5. Add that path to shell env variable `DICPATH':
;; setenv DICPATH $MYLOCAL/share/hunspell
;; 6. Restart emacs so that when hunspell is run by ispell/flyspell, that env
;; variable is effective.
;;
;; hunspell will search for a dictionary called `en_US' in the path specified by
;; `$DICPATH'

(cond
 ((executable-find "aspell")
  (setq-default ispell-program-name "aspell"))
 ((executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  ;; just reset dictionary to the safe one "en_US" for hunspell.
  ;; if we need use different dictionary, we specify it in command line arguments
  (setq-default ispell-local-dictionary "en_US")
  (setq-default ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8))))
 (t (setq-default ispell-program-name nil)))

;; ispell-cmd-args is useless, it's the list of *extra* command line arguments we will append to the ispell process when ispell-send-string()
;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
(setq-default ispell-extra-args (flyspell-detect-ispell-args t))
;; (setq ispell-cmd-args (flyspell-detect-ispell-args))
(defadvice ispell-word (around my-ispell-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

(defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use emacs original arguments
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

;; Add spell-checking in comments for all programming language modes

;(dolist (hook '(lisp-mode-hook
;                emacs-lisp-mode-hook
;                scheme-mode-hook
;                clojure-mode-hook
;                ruby-mode-hook
;                idl-mode
;                idlwave-mode
;                yaml-mode
;                python-mode-hook
;                shell-mode-hook
;                php-mode-hook
;                css-mode-hook
;                haskell-mode-hook
;                caml-mode-hook
;                c++-mode-hook
;                c-mode-hook
;                lua-mode-hook
;                crontab-mode-hook
;                perl-mode-hook
;                tcl-mode-hook
;                js2-mode-hook))
;  (add-hook hook 'flyspell-prog-mode))
;; you can also use "M-x ispell-word" or hotkey "M-$". It pop up a multiple choice
;; @see http://frequal.com/Perspectives/EmacsTip03-FlyspellAutoCorrectWord.html
(global-set-key (kbd "C-c s") 'flyspell-auto-correct-word)

(provide 'init-spelling)
