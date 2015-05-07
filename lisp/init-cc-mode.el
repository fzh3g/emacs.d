(require 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(add-hook 'irony-mode-hook
          '(lambda ()
             (require 'irony-eldoc)
             (irony-eldoc)))

(defun prelude-c-mode-common-defaults ()
  (setq-default c-default-style '((java-mode . "java")
                                  (awk-mode . "awk")
                                  (c-mode . "k&r")
                                  (c++-mode . "stroustrup")
                                  (other . "linux")))
  (setq-default c-basic-offset 4)
  (setq-default cc-search-directories
        '("." "/usr/include" "/usr/local/include/*" "../*/include"))
  (c-set-offset 'substatement-open 0))


(setq-default c-mode-common-hook 'prelude-c-mode-common-defaults)
(provide 'init-cc-mode)