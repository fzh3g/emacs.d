;;; init-sessions.el --- Emacs configuration for history and cache
;; -*- coding: utf-8 -*-
;;
;; Copyright (c) 2015-2020 Faxiang Zheng
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
      url-configuration-directory (concat fx-cache-directory "url/")
      eshell-directory-name (concat fx-cache-directory "eshell/" )
      tramp-persistency-file-name (concat fx-cache-directory "tramp"))

;; back up
(let ((backup-dir (concat fx-cache-directory "backups/")))
  (setq backup-directory-alist `(("." . ,backup-dir)))
  (unless (file-exists-p backup-dir)
    (make-directory backup-dir t)))
(setq make-backup-files t
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-old-versions 3
      kept-new-versions 2)

(setq kill-ring-max 128)

(use-package desktop
  :init
  (progn
    (setq desktop-path (list user-emacs-directory)
          desktop-auto-save-timeout 600)
    (setq desktop-save 'if-exists)
    (setq desktop-restore-frames nil)
    (setq desktop-globals-to-save
          (append '((filename-history         . 128)
                    (extended-command-history . 128)
                    (kill-ring                . 128)
                    register-alist
                    (comint-input-ring        . 128)
                    (shell-command-history    . 128)
                    (ivy-history              . 128)
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
    (desktop-save-mode 1)

    ;; desktop read time
    (defadvice desktop-read (around time-restore activate)
      (let ((start-time (current-time)))
        (prog1
            ad-do-it
          (message "Desktop restored in %.3fs"
                   (float-time (time-subtract (current-time)
                                              start-time))))))
    ;; desktop restore time
    (defadvice desktop-create-buffer (around time-create activate)
      (let ((start-time (current-time))
            (filename (ad-get-arg 1)))
        (prog1
            ad-do-it
          (message "Desktop: %.3fs to restore %s"
                   (float-time (time-subtract (current-time)
                                              start-time))
                   (when filename
                     (abbreviate-file-name filename))))))
    ))

(use-package saveplace
  :init
  (progn
    (if (fboundp 'save-place-mode)
        ;; Emacs 25 has a proper mode for `save-place'
        (save-place-mode)
      (setq save-place-mode t))
    ;; Save point position between sessions
    (setq save-place-file (concat fx-cache-directory "places"))))

(provide 'init-sessions)
;;; init-sessions.el ends here
