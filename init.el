;;; init.el --- Emacs main configuration file.
;; -*- coding: utf-8 -*-
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

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Added by Package.el.
;; (package-initialize)

;; Start time
(defconst emacs-start-time (current-time))

;; OS type const
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *linux* (eq system-type 'gnu/linux))
(defconst *win32* (eq system-type 'windows-nt))

;; Load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(defconst fx-cache-directory
  (expand-file-name ".cache/" user-emacs-directory)
  "My Emacs cache directory.")
(defconst fx-auto-save-directory
  (expand-file-name (concat fx-cache-directory "auto-save/"))
  "My Emacs auto-save directory.")
(unless (file-exists-p fx-cache-directory)
  (make-directory fx-cache-directory))
(unless (file-exists-p fx-auto-save-directory)
  (make-directory fx-auto-save-directory))

;; Debug on error
(setq debug-on-error nil)

;; silence ad-handle-definition about advised functions getting redefined
(setq ad-redefinition-action 'accept)

;; GC Optimization
(setq gc-cons-threshold (* 128 1024 1024))

;; Always load newest byte code
(setq load-prefer-newer t)

;; Bootstrap config
(require 'init-osx-keys)                ;OSX keys
(require 'init-gui-frames)              ;GUI frames
(require 'init-elpa)                    ;Install required packages
(require 'init-exec-path)               ;Set up $PATH
(require 'init-tramp)                   ;Tramp setting

;; Configs for UI and window actions
(require 'init-fullframe)
(require 'init-fonts)
(require 'init-modeline)
(require 'init-theme)
(require 'init-windows)
(require 'init-move-window-buffer)
(require 'init-uniquify)

;; Configs for some defult modes
(require 'init-ibuffer)
(require 'init-dired)
(require 'init-spelling)

;; History and desktop saving
(require 'init-recentf)
(require 'init-sessions)

;; Auto Completion
(require 'init-company)
(require 'init-hippie-expand)
(require 'init-yasnippet)
(require 'init-ivy)

;; Better settings for editing and programming
(require 'init-smartparens)
(require 'init-linum-and-scroll)
(require 'init-indentation)
(require 'init-editing-utils)
(require 'init-programming)
(require 'init-tags)
(require 'init-whitespace)
(require 'init-emoji)

;; Additional tools for efficiency
(require 'init-shell)
(require 'init-git)
(require 'init-www)

;; Languages
(require 'init-latex)
(require 'init-org)
(require 'init-python-mode)
(require 'init-fortran)
(require 'init-markdown)
(require 'init-lua-mode)
(require 'init-matlab)
(require 'init-cc-mode)
(require 'init-yaml)
(require 'init-web)
(require 'init-css)
(require 'init-vimrc)
(require 'init-idlwave)
(require 'init-lisp)

;; Variables configured via the interactive 'customize' interface
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Allow access from client
(require 'server)
(unless (server-running-p)
  (server-start))

;; Locales
(require 'init-locales)

(add-hook 'emacs-startup-hook
          (lambda ()
            (let ((elapsed (float-time
                            (time-subtract (current-time)
                                           emacs-start-time))))
              (if (file-exists-p (expand-file-name
                                  desktop-base-file-name
                                  user-emacs-directory))
                  (message
                   "Emacs loaded packages and restored desktop in %.3fs"
                   elapsed)
                (message
                 "Emacs loaded packages in %.3fs"
                 elapsed)))))

(provide 'init)
;;; init.el ends here
