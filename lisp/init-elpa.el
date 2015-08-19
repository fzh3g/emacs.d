(require 'package)

;; Standard package repositories
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

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
  '(anaconda-mode
    anzu
    auctex
    avy
    buffer-move
    cmake-mode
    color-theme-sanityinc-solarized
    company
    company-anaconda
    company-auctex
    company-c-headers
    company-irony
    company-math
    company-quickhelp
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
    flycheck-pos-tip
    guide-key
    gnuplot
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
    neotree
    nlinum
    pdf-tools
    projectile
    pyvenv
    rainbow-delimiters
    session
    smartparens
    undo-tree
    use-package
    visual-regexp
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

(require 'use-package)

(provide 'init-elpa)
