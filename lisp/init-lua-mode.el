(defun my-lua-mode-setup ()
  (interactive)
  (setq-local imenu-generic-expression '(("Variable" "^ *\\([a-zA-Z0-9_.]+\\) *= *{ *[^ ]*$" 1)
                                         ("Function" "function +\\([^ (]+\\).*$" 1)
                                         ("Module" "^ *module +\\([^ ]+\\) *$" 1)
                                         ("Variable" "^ *local +\\([^ ]+\\).*$" 1)))
  (setq safe-local-variable-values
        '((lua-indent-level . 2)
          (lua-indent-level . 3)
          (lua-indent-level . 4)
          (lua-indent-level . 8))))

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(add-hook 'lua-mode-hook 'my-lua-mode-setup)

(provide 'init-lua-mode)
