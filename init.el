;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

(add-to-list 'load-path "~/.emacs.d/lisp/")


;;----------------------------------------------------------------------
;; Less GC, more memory
;;----------------------------------------------------------------------
;; By default Emacs will initiate GC every 0.76 MB allocated
;; (gc-cons-threshold == 800000).
;; we increase this to 1GB (gc-cons-threshold == 100000000)
;; @see http://www.gnu.org/software/emacs/manual/html_node/elisp/Garbage-Collection.html
(setq-default gc-cons-threshold 100000000
              gc-cons-percentage 0.5)

;;----------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------
;(require 'init-site-lisp)
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

;;----------------------------------------------------------------------
;; Load configs for spesific features and modes
;;----------------------------------------------------------------------
(require 'init-modeline)
(require 'init-frame-hooks)
(require 'init-theme)
(require 'init-xterm)
(require 'init-gui-frames)
(require 'init-windows)
(require 'init-move-window-buffer)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-sessions)
(require 'init-dired)
(require 'init-isearch)
(require 'init-recentf)
(require 'init-spelling)
(require 'init-yasnippet)
(require 'init-company)
(require 'init-ido)
(require 'init-helm)
(require 'init-hippie-expand)
(require 'init-programming)
(require 'init-org)
(require 'init-LaTeX)
(require 'init-python-mode)
(require 'init-cc-mode)
(require 'init-shell)
(require 'init-editing-utils)
(require 'init-smartparens)
(require 'init-gnuplot)
(require 'init-markdown)

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
