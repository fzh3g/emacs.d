;;; init-indentation.el --- Emacs configuration for indentation
;; -*- coding: utf-8 -*-
;;
;; Copyright (c) 2015-2017 Faxiang Zheng
;;
;; Author: Faxiang Zheng <fxzheng0906@outlook.com>
;; URL: https://github.com/zhengfaxiang/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Some configuration for indentation.

;;; Code:

;; indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(eval-after-load "js"
  '(setq-default js-indent-level 2))

(eval-after-load 'css-mode
  '(setq-default css-indent-offset 2))

(eval-after-load 'yaml-mode
  '(setq-default yaml-indent-offset 2))

(eval-after-load 'web-mode
  '(setq-default web-mode-markup-indent-offset 2
                 web-mode-css-indent-offset 2
                 web-mode-code-indent-offset 2
                 web-mode-attr-indent-offset 2))

(define-key global-map(kbd "RET") 'newline-and-indent)

;; taken from Prelude
(defmacro fx|advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.
The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command
                      (,class ,(intern (concat
                                        (symbol-name command)
                                        "-"
                                        advice-name))
                              activate)
                    ,@body))
               commands)))

(defvar fx-indent-sensitive-modes
  '(coffee-mode
    elm-mode
    haml-mode
    haskell-mode
    slim-mode
    makefile-mode
    makefile-bsdmake-mode
    makefile-gmake-mode
    makefile-imake-mode
    python-mode)
  "Modes for which auto-indenting is suppressed.")

(defcustom fx-yank-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur."
  :type 'number
  :group 'fx)

(defcustom fx-yank-indent-modes '(LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here."
  :type 'list
  :group 'fx)

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode fx-indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (indent-region (point-min) (point-max))
          (message "Indented buffer.")))
      (whitespace-cleanup))))
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

;; indent on paste
(defun fx/yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) fx-yank-indent-threshold)
      (indent-region beg end nil)))

(fx|advise-commands
 "indent" (yank yank-pop) after
 "If current mode is one of `fx-yank-indent-modes',
indent yanked text (with prefix arg don't indent)."
 (if (and (not (ad-get-arg 0))
          (not (member major-mode fx-indent-sensitive-modes))
          (or (derived-mode-p 'prog-mode)
              (member major-mode fx-yank-indent-modes)))
     (let ((transient-mark-mode nil))
       (fx/yank-advised-indent-function (region-beginning) (region-end)))))

(provide 'init-indentation)
;;; init-indentation.el ends here
