;;; init-shell.el --- Emacs configuration for Shell
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

;; Some configuration for eshell and multi-term.

;;; Code:

;; move point to the end of buffer on new output
(setq comint-move-point-for-output t)

(defvar fx-default-shell (if *win32*
                             'eshell
                           'ansi-term)
  "Default shell to use in Emacs.  Possible values are `eshell', `shell',
`term', `ansi-term' and `multi-term'.")

;; eshell
(use-package eshell
  :defer t
  :init
  (progn
    (setq eshell-cmpl-cycle-completions nil
          ;; auto truncate after 20k lines
          eshell-buffer-maximum-lines 20000
          ;; history size
          eshell-history-size 350
          ;; no duplicates in history
          eshell-hist-ignoredups t
          ;; buffer shorthand -> echo foo > #'buffer
          eshell-buffer-shorthand t
          ;; my prompt is easy enough to see
          eshell-highlight-prompt nil
          ;; treat 'echo' like shell echo
          eshell-plain-echo-behavior t))
  :config
  (progn
    (require 'esh-opt)

    (require 'em-term)
    (mapc (lambda (x) (push x eshell-visual-commands))
          '("el" "elinks" "htop" "less" "ssh" "tmux" "top"))

    (when (boundp 'eshell-output-filter-functions)
      (push 'eshell-truncate-buffer
            eshell-output-filter-functions))))

(use-package eshell-prompt-extras
  :commands epe-theme-lambda
  :init
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

(use-package xterm-color
  :init
  (progn
    ;; Comint and Shell
    (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
    (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions))
    (setq font-lock-unfontify-region-function 'xterm-color-unfontify-region)
    (with-eval-after-load 'esh-mode
      (add-hook 'eshell-mode-hook (lambda () (setq xterm-color-preserve-properties t)))
      (add-hook 'eshell-preoutput-filter-functions 'xterm-color-filter)
      (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))))

(use-package multi-term
  :defer t
  :init
  (progn
    (defun multiterm (_)
      "Wrapper to be able to call multi-term from shell-pop"
      (interactive)
      (multi-term)))
  :config
  (progn
    (add-hook 'term-mode-hook
              #'(lambda ()
                  (define-key term-raw-map (kbd "C-c C-t") 'multi-term)))

    (defun term-send-tab ()
      "Send tab in term mode."
      (interactive)
      (term-send-raw-string "\t"))
    (add-to-list 'term-bind-key-alist '("<tab>" . term-send-tab))

    (defun projectile-multi-term-in-root ()
      "Invoke `multi-term' in the project's root."
      (interactive)
      (projectile-with-default-dir (projectile-project-root) (multi-term)))
    (global-set-key (kbd "C-c p C-t") 'projectile-multi-term-in-root)))

(use-package shell-pop
  :defer t
  :init
  (progn
    (setq shell-pop-window-position 'bottom
          shell-pop-window-height 30
          shell-pop-term-shell shell-file-name
          shell-pop-full-span t)
    (defmacro make-shell-pop-command (type &optional shell)
      (let* ((name (symbol-name type)))
        `(defun ,(intern (concat "shell-pop-" name)) (index)
           (interactive "P")
           (require 'shell-pop)
           (shell-pop--set-shell-type
            'shell-pop-shell-type
            (backquote (,name
                        ,(concat "*" name "*")
                        (lambda nil (funcall ',type ,shell)))))
           (shell-pop index))))
    (make-shell-pop-command eshell)
    (make-shell-pop-command shell)
    (make-shell-pop-command term shell-pop-term-shell)
    (make-shell-pop-command multiterm)
    (make-shell-pop-command ansi-term shell-pop-term-shell)

    (defun ansi-term-handle-close ()
      "Close current term buffer when `exit' from term buffer."
      (when (ignore-errors (get-buffer-process (current-buffer)))
        (set-process-sentinel (get-buffer-process (current-buffer))
                              (lambda (proc change)
                                (when (string-match "\\(finished\\|exited\\)" change)
                                  (kill-buffer (process-buffer proc))
                                  (delete-window))))))
    (add-hook 'term-mode-hook 'ansi-term-handle-close)

    (defun fx/default-pop-shell ()
      "Open the default shell in a popup."
      (interactive)
      (let ((shell (if (eq 'multi-term fx-default-shell)
                       'multiterm
                     fx-default-shell)))
        (call-interactively (intern (format "shell-pop-%S" shell)))))
    (global-set-key (kbd "C-x t '") #'fx/default-pop-shell)
    (global-set-key (kbd "C-x t e") #'shell-pop-eshell)
    (global-set-key (kbd "C-x t s") #'shell-pop-shell)
    (global-set-key (kbd "C-x t m") #'shell-pop-multiterm)
    (global-set-key (kbd "C-x t t") #'shell-pop-ansi-term)
    (global-set-key (kbd "C-x t T") #'shell-pop-term)))

(defun shell-comint-input-sender-hook ()
  "Check certain shell commands.
Executes the appropriate behavior for certain commands."
  (setq comint-input-sender
        (lambda (proc command)
          (cond
           ;; Check for clear command and execute it.
           ((string-match "^[ \t]*clear[ \t]*$" command)
            (comint-send-string proc "\n")
            (erase-buffer))
           ;; Check for man command and execute it.
           ((string-match "^[ \t]*man[ \t]*" command)
            (comint-send-string proc "\n")
            (setq command (replace-regexp-in-string "^[ \t]*man[ \t]*" "" command))
            (setq command (replace-regexp-in-string "[ \t]+$" "" command))
            (funcall 'man command))
           ;; Send other commands to the default handler.
           (t (comint-simple-send proc command))))))
(add-hook 'shell-mode-hook 'shell-comint-input-sender-hook)

(provide 'init-shell)
;;; init-shell.el ends here
