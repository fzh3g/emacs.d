;; anaconda mode
(use-package anaconda-mode
  :defer t
  :init
  (add-hook 'python-mode-hook 'anaconda-mode))

(use-package pyvenv
  :defer t)

(use-package python
  :defer t
  :init
  (progn
    (defun python-default ()
      (setq python-indent 4
            python-indent-offset 4
            python-indent-guess-indent-offset nil
            python-guess-indent nil)
      (local-set-key (kbd "C-j") 'newline-and-indent))

    (defun python-setup-shell ()
      (if (executable-find "ipython")
          (setq python-shell-interpreter "ipython"
                python-shell-interpreter-args " "
                python-shell-prompt-regexp "In \\[[0-9]+\\]: "
                python-shell-prompt-output-regexp "Out\\[[0-9]+]: "
                python-shell-completion-setup-code
                "from IPython.core.completerlib import module_completion"
                python-shell-completion-module-string-code
                "';'.join(module_completion('''%s'''))\n"
                python-shell-completion-string-code
                "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
        (setq python-shell-interpreter "python")))

    (add-hook 'python-mode-hook
              '(lambda ()
                 (python-default)
                 (python-setup-shell)))))

(provide 'init-python-mode)
