;;; init-python-mode.el --- Emacs configuration for Python
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

;; Some configuration for python.

;;; Code:

;; jedi
(use-package jedi-core
  :defer t
  :init
  (progn
    (setq jedi:complete-on-dot t)
    (setq jedi:use-shortcuts t)
    (add-hook 'python-mode-hook
              #'(lambda ()
                  (jedi:setup)))))

(use-package pyvenv
  :defer t)

(use-package python
  :defer t
  :init
  (progn
    (defun fx/python-default ()
      (setq tab-width 4
            fill-column 79)
      (setq-local comment-inline-offset 2)
      (local-set-key (kbd "C-j") 'newline-and-indent))

    (defun fx/pyenv-executable-find (command)
      "Find executable taking pyenv shims into account."
      (if (executable-find "pyenv")
          (progn
            (let ((pyenv-string (shell-command-to-string
                                 (concat "pyenv which " command))))
              (unless (string-match "not found" pyenv-string)
                pyenv-string)))
        (executable-find command)))

    (defun fx/python-setup-shell (&rest args)
      (if (fx/pyenv-executable-find "ipython3")
          (progn (setq python-shell-interpreter "ipython")
                 (setq python-shell-interpreter-args "--simple-prompt -i"))
        (progn
          (setq python-shell-interpreter-args "-i")
          (setq python-shell-interpreter "python3")))
      (setq python-shell-completion-native-enable nil))

    (defun fx/inferior-python-shell-setup ()
      (setq indent-tabs-mode t))

    (add-hook 'python-mode-hook #'fx/python-default)
    (add-hook 'inferior-python-mode-hook #'fx/inferior-python-shell-setup)

    (fx/python-setup-shell))
  :config
  (progn
    (eval-after-load 'company
      '(add-hook 'inferior-python-mode-hook
                (lambda ()
                  (setq-local company-minimum-prefix-length 0)
                  (setq-local company-idle-delay 0.5))))))

(provide 'init-python-mode)
;;; init-python-mode.el ends here
