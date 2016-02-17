(use-package vimrc-mode
  :mode "\\.vim[rc]?\\'"
  :mode "_vimrc\\'"
  :defer t
  :init
  (progn
    (add-hook 'vimrc-mode-hook
              '(lambda ()
                 (rainbow-delimiters-mode-disable)))))

(provide 'init-vimrc)
