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

;; Some configuration for ggtags and ctags.

;;; Code:

(use-package etags
  :config
  (progn
    ;; Don't ask before rereading the TAGS files if they have changed
    (setq tags-revert-without-query t)
    ;; Do case-sensitive tag searches
    (setq tags-case-fold-search nil)))

(use-package etags-select
  :bind (("M-s d" . etags-select-find-tag-at-point)
         ("M-s f" . etags-select-find-tag)))

(provide 'init-tags)
;;; init-tags.el ends here
