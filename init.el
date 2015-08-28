;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.
;;----------------------------------------------------------------------
;; OS type const
;;----------------------------------------------------------------------
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *linux* (eq system-type 'gnu/linux))
(defconst *win32* (eq system-type 'windows-nt))
(defconst *cygwin* (eq system-type 'cygwin))
;;----------------------------------------------------------------------
;; load-path
;;----------------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(defconst fx-cache-directory
  (expand-file-name ".cache/" user-emacs-directory))
;;----------------------------------------------------------------------
;; debug on error
;;----------------------------------------------------------------------
(setq debug-on-error nil)
;;----------------------------------------------------------------------
;; GC Optimization
;;----------------------------------------------------------------------
(setq gc-cons-threshold 20000000)
;;----------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH
(require 'init-tramp)
;;----------------------------------------------------------------------
;; Load configs for spesific features and modes
;;----------------------------------------------------------------------
(require 'init-frame-hooks)
(require 'init-theme)
(require 'init-modeline)
(require 'init-osx-keys)
(require 'init-xterm)
(require 'init-gui-frames)
(require 'init-windows)
(require 'init-move-window-buffer)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-dired)
(require 'init-isearch)
(require 'init-recentf)
(require 'init-spelling)
(require 'init-smartparens)
(require 'init-company)
(require 'init-hippie-expand)
(require 'init-yasnippet)
(require 'init-ido)
(require 'init-helm)
(require 'init-editing-utils)
(require 'init-programming)
(require 'init-org)
(require 'init-latex)
(require 'init-python-mode)
(require 'init-fortran)
(require 'init-shell)
(require 'init-regexp)
(require 'init-gnuplot)
(require 'init-markdown)
(require 'init-lua-mode)
(require 'init-matlab)
(require 'init-gtags)
(require 'init-sessions)
(require 'init-git)
(require 'init-cc-mode)
(require 'init-yaml)
(require 'init-web)
(require 'init-css)
;;----------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
;;----------------------------------------------------------------------
;; Allow access from client
;;----------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))
;;----------------------------------------------------------------------
;; Locales
;;----------------------------------------------------------------------
(require 'init-locales)

(provide 'init)
;;; init.el ends here
