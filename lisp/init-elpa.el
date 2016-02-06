(require 'package)

;; Standard package repositories
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

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

;; list of installed packages
(defvar fx-installed-packages
  '(anzu
    auctex
    avy
    buffer-move
    cmake-mode
    company
    company-auctex
    company-c-headers
    company-irony
    company-jedi
    company-statistics
    diff-hl
    dired+
    exec-path-from-shell
    expand-region
    fill-column-indicator
    flx
    flx-ido
    flycheck
    flycheck-pos-tip
    gnuplot
    helm
    helm-ag
    helm-c-yasnippet
    helm-swoop
    highlight-symbol
    htmlize
    ibuffer-vc
    ido-completing-read+
    ido-hacks
    idomenu
    ido-ubiquitous
    ido-vertical-mode
    indent-guide
    irony
    irony-eldoc
    jedi-core
    less-css-mode
    lua-mode
    magit
    markdown-mode
    matlab-mode
    mmm-mode
    multiple-cursors
    neotree
    nlinum
    org-plus-contrib
    org-bullets
    page-break-lines
    paradox
    projectile
    pyvenv
    rainbow-delimiters
    sass-mode
    scss-mode
    session
    smartparens
    solarized-theme
    undo-tree
    use-package
    visual-regexp
    web-mode
    window-numbering
    which-key
    yaml-mode
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

(use-package paradox
  :defer t)

(provide 'init-elpa)
