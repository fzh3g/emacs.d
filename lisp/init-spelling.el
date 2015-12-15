(use-package flyspell
  :defer t
  :init
  (progn
    (dolist (hook '(text-mode-hook TeX-mode-hook org-mode-hook markdown-mode-hook))
      (add-hook hook 'flyspell-mode))
    ;; better performance
    (setq flyspell-issue-message-flag nil)
    (cond
     ((executable-find "aspell")
      (setq ispell-program-name "aspell"))
     ((executable-find "hunspell")
      (setq ispell-program-name "hunspell")
      ;; just reset dictionary to the safe one "en_US" for hunspell.
      ;; if we need use different dictionary, we specify it in command line arguments
      (setq ispell-local-dictionary "en_US")
      (setq ispell-local-dictionary-alist
                    '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8))))
     (t (setq ispell-program-name nil)))))

(provide 'init-spelling)
