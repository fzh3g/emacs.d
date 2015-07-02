;; anaconda mode
(require 'anaconda-mode)
(add-hook 'python-mode-hook
          '(lambda ()
             (anaconda-mode)
             ))

;;----------------------------------------------------------------------
;; python shell
;;----------------------------------------------------------------------
(setq-default
 python-shell-interpreter "ipython"
 python-shell-interpreter-args " "
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")


(provide 'init-python-mode)
;;; init-python-mode.el ends here
