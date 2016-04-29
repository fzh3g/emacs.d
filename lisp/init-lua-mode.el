(use-package lua-mode
  :defer t
  :mode "\\.lua\\'"
  :interpreter "lua"
  :config
  (progn
    (setq lua-indent-level 4
          lua-indent-string-contents t)
    (setq-local imenu-generic-expression
              '(("Variable" "^ *\\([a-zA-Z0-9_.]+\\) *= *{ *[^ ]*$" 1)
                ("Function" "function +\\([^ (]+\\).*$" 1)
                ("Module" "^ *module +\\([^ ]+\\) *$" 1)
                ("Variable" "^ *local +\\([^ ]+\\).*$" 1)))))

(provide 'init-lua-mode)
