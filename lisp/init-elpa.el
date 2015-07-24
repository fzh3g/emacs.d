(require 'package)

;; Standard package repositories
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; If gpg cannot be found, signature checking will fail, so we
;; conditionally enable it according to whether gpg is available. We
;; re-run this check once $PATH has been configured
(defun sanityinc/package-maybe-enable-signatures ()
  (setq package-check-signature (when (executable-find "gpg") nil)))

(sanityinc/package-maybe-enable-signatures)
(eval-after-load 'init-exec-path
  '(progn
     (sanityinc/package-maybe-enable-signatures)))


;; Fire up package.el

(setq package-enable-at-startup nil)
(package-initialize)

;; list of installed packages
(defvar fx-installed-packages
  '(ace-jump-mode
    anaconda-mode
    anzu
    auctex
    buffer-move
    cdlatex
    company
    company-anaconda
    company-auctex
    company-c-headers
    company-cmake
    company-irony
    company-math
    company-statistics
    cyberpunk-theme
    diff-hl
    dired+
    exec-path-from-shell
    expand-region
    fill-column-indicator
    flx
    flx-ido
    flycheck
    guide-key
    helm
    helm-ag
    helm-gtags
    helm-swoop
    highlight-indentation
    highlight-symbol
    ibuffer-vc
    ido-completing-read+
    ido-hacks
    idomenu
    ido-ubiquitous
    ido-vertical-mode
    irony
    irony-eldoc
    lua-mode
    magit
    markdown-mode
    math-symbol-lists
    matlab-mode
    multiple-cursors
    nlinum
    projectile
    pyvenv
    rainbow-delimiters
    session
    smartparens
    sr-speedbar
    undo-tree
    visual-regexp-steroids
    window-numbering
    yasnippet)
  "A list of packages to ensure are installed at launch.")


; fetch the list of packages available
(unless package-archive-contents
    (package-refresh-contents))

;; Install a package only if it's not already installed.
(dolist (pkg fx-installed-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))


(provide 'init-elpa)
