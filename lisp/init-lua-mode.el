(defun my-lua-mode-setup ()
  (interactive)
  (setq-default lua-indent-level 4)
  (setq-local imenu-generic-expression
              '(("Variable" "^ *\\([a-zA-Z0-9_.]+\\) *= *{ *[^ ]*$" 1)
                ("Function" "function +\\([^ (]+\\).*$" 1)
                ("Module" "^ *module +\\([^ ]+\\) *$" 1)
                ("Variable" "^ *local +\\([^ ]+\\).*$" 1))))


(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(add-hook 'lua-mode-hook 'my-lua-mode-setup)

(provide 'init-lua-mode)
