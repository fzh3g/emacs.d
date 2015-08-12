(use-package visual-regexp
  :defer t
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)
         ("C-c m" . vr/mc-mark)))

(provide 'init-regexp)
;;; init-regexp.el ends here
