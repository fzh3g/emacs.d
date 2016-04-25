(use-package visual-regexp
  :defer t
  :bind (("C-c v r" . vr/replace)
         ("C-c v q" . vr/query-replace)
         ("C-c v m" . vr/mc-mark)))

(provide 'init-regexp)
;;; init-regexp.el ends here
