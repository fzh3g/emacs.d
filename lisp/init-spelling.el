(dolist (hook '(text-mode-hook TeX-mode-hook org-mode-hook markdown-mode-hook))
  (add-hook hook 'flyspell-mode))

;; better performance
(setq-default flyspell-issue-message-flag nil)

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

;; @see http://frequal.com/Perspectives/EmacsTip03-FlyspellAutoCorrectWord.html
(global-set-key (kbd "C-c s") 'flyspell-auto-correct-word)

(provide 'init-spelling)
