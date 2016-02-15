(require 'init-frame-hooks)

(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))

(defun sanityinc/console-frame-setup ()
    (xterm-mouse-mode 1) ; Mouse in a terminal (Use shift to paste with middle button)
  (when (fboundp 'mwheel-install)
    (mwheel-install)))

(add-hook 'after-make-console-frame-hooks 'sanityinc/console-frame-setup)

(provide 'init-xterm)
