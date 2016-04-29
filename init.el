;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.
;;--------------------------------------------------------------------
;; Added by Package.el.
;;--------------------------------------------------------------------
;; (package-initialize)
;;--------------------------------------------------------------------
;; OS type const
;;--------------------------------------------------------------------
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *linux* (eq system-type 'gnu/linux))
(defconst *win32* (eq system-type 'windows-nt))
;;--------------------------------------------------------------------
;; Load-path
;;--------------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(defconst fx-cache-directory
  (expand-file-name ".cache/" user-emacs-directory))
(if (not (file-exists-p fx-cache-directory))
    (make-directory fx-cache-directory))
;;--------------------------------------------------------------------
;; Debug on error
;;--------------------------------------------------------------------
(setq debug-on-error nil)
;;--------------------------------------------------------------------
;; GC Optimization
;;--------------------------------------------------------------------
(setq gc-cons-threshold 20000000)
;;--------------------------------------------------------------------
;; Bootstrap config
;;--------------------------------------------------------------------
(require 'init-elpa)       ;; Install required packages
(require 'init-exec-path)  ;; Set up $PATH
(require 'init-tramp)      ;; Tramp setting
;;--------------------------------------------------------------------
;; Configs for UI and window actions
;;--------------------------------------------------------------------
(require 'init-osx-keys)
(require 'init-gui-frames)
(require 'init-fonts)
(require 'init-modeline)
(require 'init-theme)
(require 'init-windows)
(require 'init-move-window-buffer)
(require 'init-uniquify)
(require 'init-smartparens)
;;--------------------------------------------------------------------
;; Configs for some defult modes
;;--------------------------------------------------------------------
(require 'init-ibuffer)
(require 'init-dired)
(require 'init-isearch)
(require 'init-spelling)
;;--------------------------------------------------------------------
;; History and desktop saving
;;--------------------------------------------------------------------
(require 'init-recentf)
(require 'init-sessions)
;;--------------------------------------------------------------------
;; Auto Completion
;;--------------------------------------------------------------------
(require 'init-company)
(require 'init-hippie-expand)
(require 'init-yasnippet)
(require 'init-ido)
(require 'init-helm)
;;--------------------------------------------------------------------
;; Better settings for editing and programming
;;--------------------------------------------------------------------
(require 'init-editing-utils)
(require 'init-programming)
;;--------------------------------------------------------------------
;; Additional tools for efficiency
;;--------------------------------------------------------------------
(require 'init-regexp)
(require 'init-shell)
(require 'init-git)
;;--------------------------------------------------------------------
;; Languages
;;--------------------------------------------------------------------
(require 'init-org)
(require 'init-latex)
(require 'init-python-mode)
(require 'init-fortran)
(require 'init-gnuplot)
(require 'init-markdown)
(require 'init-lua-mode)
(require 'init-matlab)
(require 'init-cc-mode)
(require 'init-yaml)
(require 'init-web)
(require 'init-css)
(require 'init-vimrc)
;;--------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;--------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
;;--------------------------------------------------------------------
;; Allow access from client
;;--------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))
;;--------------------------------------------------------------------
;; Locales
;;--------------------------------------------------------------------
(require 'init-locales)

(provide 'init)
;;; init.el ends here
