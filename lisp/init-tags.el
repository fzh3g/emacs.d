;;; init-tags.el --- Emacs configuration for tags
;;
;; Copyright (c) 2015-2016 Faxiang Zheng
;;
;; Author: Faxiang Zheng <fxzheng0906@outlook.com>
;; URL: https://github.com/zhengfaxiang/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Some configuration for ctags.

;;; Code:

(defun fx/setup-ctags ()
  (use-package etags
    :init
    (progn
      ;; Don't ask before rereading the TAGS files if they have changed
      (setq tags-revert-without-query t)
      ;; Do case-sensitive tag searches
      (setq tags-case-fold-search nil)))

  (use-package etags-select
    :init
    (progn
      (bind-key "M-s f" 'etags-select-find-tag)
      (bind-key "M-\." 'etags-select-find-tag-at-point)
      (bind-key "M-\," 'pop-tag-mark)))

  (use-package ctags-update
    :diminish (ctags-auto-update-mode . " τ")
    :init (turn-on-ctags-auto-update-mode)))

(dolist (hook '(c-mode-hook
                c++-mode-hook
                matlab-mode-hook
                idlwave-mode-hook))
  (add-hook hook #'fx/setup-ctags))

(provide 'init-tags)
;;; init-tags.el ends here
