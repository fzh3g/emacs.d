;;; init-locales.el --- Emacs configuration for locales
;; -*- coding: utf-8 -*-
;;
;; Copyright (c) 2015-2018 Faxiang Zheng
;;
;; Author: Faxiang Zheng <fxzheng0906@outlook.com>
;; URL: https://github.com/zhengfaxiang/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Some configuration for locales.

;;; Code:

(defun fx/utf8-locale-p (v)
  "Return whether locale string V relates to a UTF-8 locale."
  (and v (string-match "UTF-8" v)))

(defun fx/locale-is-utf8-p ()
  "Return t if the \"locale\" command or environment variables prefer UTF-8."
  (or (fx/utf8-locale-p (and (executable-find "locale") (shell-command-to-string "locale")))
      (fx/utf8-locale-p (getenv "LC_ALL"))
      (fx/utf8-locale-p (getenv "LC_CTYPE"))
      (fx/utf8-locale-p (getenv "LANG"))))

(when (or window-system (fx/locale-is-utf8-p))
  (setq locale-coding-system 'utf-8)
  (unless (eq system-type 'windows-nt)
    (set-selection-coding-system 'utf-8))
  (prefer-coding-system 'utf-8))

(provide 'init-locales)
;;; init-locales.el ends here
