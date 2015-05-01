(require 'highlight-indentation)

(add-hook 'prog-mode-hook 'highlight-indentation-mode)

;(add-hook 'prog-mode-hook 'highlight-indentation-current-column-mode)

(set-face-background 'highlight-indentation-face "#e3e3d3")
(set-face-background 'highlight-indentation-current-column-face "#c3b3b3")

(provide 'init-highlight-indentation)
