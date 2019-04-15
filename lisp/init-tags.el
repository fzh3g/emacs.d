;;; init-tags.el --- Emacs configuration for tags
;; -*- coding: utf-8 -*-
;;
;; Copyright (c) 2015-2019 Faxiang Zheng
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
      (setq tags-revert-without-query t)))

  (use-package counsel-etags
    :init
    (progn
      (local-set-key (kbd "M-s f") 'counsel-etags-find-tag)
      (local-set-key (kbd "M-\.") 'counsel-etags-find-tag-at-point)
      (local-set-key (kbd "M-\,") 'pop-tag-mark)))

  (use-package ctags-update
    :diminish (ctags-auto-update-mode . " τ")
    ;; :init (turn-on-ctags-auto-update-mode)
    ))

(dolist (hook '(c-mode-hook
                c++-mode-hook
                matlab-mode-hook
                idlwave-mode-hook))
  (add-hook hook #'fx/setup-ctags))

(provide 'init-tags)
;;; init-tags.el ends here
