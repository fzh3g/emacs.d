;;;
;;----------------------------------------------------------------------
;; elpy
;;----------------------------------------------------------------------
(require 'elpy)
(elpy-enable)
(elpy-use-ipython)
(setq elpy-rpc-backend "jedi")
(add-hook 'pyvenv-post-activate-hook 'pyvenv-restart-python)

;;----------------------------------------------------------------------
;; ipython notebook
;;----------------------------------------------------------------------
(require 'ein)
(setq ein:complete-on-dot t)
(setq ein:notebook-modes '(ein:notebook-mumamo-mode ein:notebook-python-mode))
(setq ein:query-timeout 1000)


;;----------------------------------------------------------------------
;; python shell
;;----------------------------------------------------------------------
;(require 'python)
;(setq
; python-shell-interpreter "ipython"
; python-shell-interpreter-args " "
; python-shell-prompt-regexp "In \\[[0-9]+\\]: "
; python-shell-prompt-output-regexp "Out\\[[0-9]+]: "
; python-shell-completion-setup-code
;   "from IPython.core.completerlib import module_completion"
; python-shell-completion-module-string-code
;   "';'.join(module_completion('''%s'''))\n"
; python-shell-completion-string-code
;   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")


(provide 'init-python-mode)
;;; init-python-mode.el ends here
