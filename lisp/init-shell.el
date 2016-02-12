; http://www.emacswiki.org/emacs/ShellMode
(global-set-key (kbd "C-x t") 'alt-shell-dwim)
(defun alt-shell-dwim (arg)
  "Run an inferior shell like `shell'. If an inferior shell as its I/O
 through the current buffer, then pop the next buffer in `buffer-list'
 whose name is generated from the string \"*shell*\". When called with
 an argument, start a new inferior shell whose I/O will go to a buffer
 named after the string \"*shell*\" using `generate-new-buffer-name'."
  (interactive "P")
  (let* ((shell-buffer-list
          (let (blist)
            (dolist (buff (buffer-list) blist)
              (when (string-match "^\\*shell\\*" (buffer-name buff))
                (setq blist (cons buff blist))))))
         (name (if arg
                   (generate-new-buffer-name "*shell*")
                 (car shell-buffer-list))))
    (shell name)))

;; ansi color
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

; http://stackoverflow.com/questions/25094855/visible-ansi-escape-sequences-when-running-emacs-shell-these-are-not-ansi-color
;; add following to ~/.zshrc
;;if [[ -n $EMACS]]; then
;;    export TERM=dumb
;;else
;;    export TERM=xterm-256color
;;fi

(add-to-list 'auto-mode-alist '("\\.*conf\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.profile\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash_history\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zshrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zsh_history\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

(provide 'init-shell)
