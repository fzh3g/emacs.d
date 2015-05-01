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

;; anaconda

;(add-hook 'python-mode-hook
;          '(lambda ()
;	     (require 'pyenv-mode)
;	     (pyenv-mode)
;	     (anaconda-mode)
;	     (add-to-list 'company-backends 'company-anaconda)
;	     (eldoc-mode)
;	     ))

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

;;----------------------------------------------------------------------
;; jedi
;;----------------------------------------------------------------------
;(add-hook
; 'after-init-hook
;  '(lambda()
;     (require 'jedi)
;     (add-hook 'python-mode-hook 'jedi:setup)
;     ;; custom keybindings
;     (defun jedi-config:setup-keys ()
;       (local-set-key (kbd "M-.") 'jedi:goto-definition)
;       (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)
;       (local-set-key (kbd "M-?") 'jedi:show-doc)
;       (local-set-key (kbd "M-/") 'jedi:get-in-function-call))
;     ;; https://github.com/wernerandrew/jedi-starter/blob/master/jedi-starter.el
;     (add-hook 'python-mode-hook 'jedi-config:setup-keys)
;     (setq jedi:complete-on-dot t)
;     ;(setq jedi:get-in-function-call-delay 10000000)
;     ;(setq jedi:get-in-function-call-timeout 3000)
;     ;(setq jedi:environment-root nil)
;     ;(setq jedi:environment-virtualenv nil)
;
;     (defvar jedi-config:use-system-python nil
;       "Will use system python and active environment for Jedi server.
;May be necessary for some GUI environments (e.g., Mac OS X)")
;
;     (defvar jedi-config:with-virtualenv nil
;       "Set to non-nil to point to a particular virtualenv.")
;     ))
;
;;;----------------------------------------------------------------------
;;; jedi-direx
;;;----------------------------------------------------------------------
;(require 'jedi-direx)
;(eval-after-load "python"
;  '(define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer))
;(add-hook 'jedi-mode-hook 'jedi-direx:setup)

(provide 'init-python-mode)
;;; init-python-mode.el ends here
