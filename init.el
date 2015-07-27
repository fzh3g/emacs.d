;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *linux* (eq system-type 'gnu/linux))
(defconst *win32* (eq system-type 'windows-nt))
(defconst *cygwin* (eq system-type 'cygwin))

(when *win32*
  (setenv "HOME" "c:/cygwin64/home/Faxiang"))

(add-to-list 'load-path "~/.emacs.d/lisp/")
;;----------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH
;;----------------------------------------------------------------------
;; Load configs for spesific features and modes
;;----------------------------------------------------------------------
(require 'init-modeline)
(require 'init-frame-hooks)
(require 'init-theme)
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
(require 'init-yasnippet)
(require 'init-company)
(require 'init-ido)
(require 'init-hippie-expand)
(require 'init-programming)
(require 'init-org)
(require 'init-latex)
(require 'init-cc-mode)
(require 'init-fortran)
(require 'init-shell)
(require 'init-editing-utils)
(require 'init-regexp)
(require 'init-smartparens)
(require 'init-gnuplot)
(require 'init-markdown)
(require 'init-lua-mode)
(require 'init-matlab)
(require 'init-python-mode)
(require 'init-helm)
(require 'init-gtags)
(require 'init-sessions)
(require 'init-git)

;;----------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------
(setq custom-file "~/.emacs.d/custom.el")
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
