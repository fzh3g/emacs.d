;;; init-sessions.el --- Emacs configuration for history and cache
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

;; Some configuration for cache file, session, desktop save and history.

;;; Code:

;; cache files
(setq bookmark-default-file (concat fx-cache-directory "bookmarks")
      bookmark-save-flag 1
      url-configuration-directory (concat fx-cache-directory "url")
      eshell-directory-name (concat fx-cache-directory "eshell" )
      tramp-persistency-file-name (concat fx-cache-directory "tramp"))

;; back up
(if (not (file-exists-p (expand-file-name "~/.backups")))
    (make-directory (expand-file-name "~/.backups")))
(setq make-backup-files t
      backup-directory-alist `(("." . "~/.backups"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-old-versions 3
      kept-new-versions 2)

(setq kill-ring-max 128)

(setq session-save-file (concat fx-cache-directory ".session"))
(setq session-save-print-spec '(t nil 40000))

(use-package desktop
  :init
  (progn
    (setq desktop-path (list fx-cache-directory))
    (setq desktop-save 'if-exists)
    (setq desktop-globals-to-save
          (append '((ido-last-directory-list  . 128)
                    (ido-work-directory-list  . 128)
                    (ido-file-history         . 128)
                    (filename-history         . 128)
                    (extended-command-history . 128)
                    (kill-ring                . 128)
                    register-alist
                    (comint-input-ring        . 128)
                    (shell-command-history    . 128)
                    (read-expression-history  . 64)
                    (minibuffer-history       . 64)
                    (regexp-history           . 64)
                    (query-replace-history    . 64)
                    (org-tags-history         . 64)
                    (org-clock-history        . 64)
                    (compile-history          . 32)
                    (search-ring              . 32)
                    (regexp-search-ring       . 32)
                    desktop-missing-file-warning
                    )))
    (desktop-save-mode 1)))

(provide 'init-sessions)
;;; init-sessions.el ends here
