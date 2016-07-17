(require 'package)

;; Standard package repositories
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(setq package-archives
      '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("org"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

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
(package-initialize)
(setq package-enable-at-startup nil)

;; list of installed packages
(defvar fx-packages
  '(anzu
    auctex
    avy
    buffer-move
    cmake-mode
    company
    company-auctex
    company-c-headers
    company-emoji
    company-irony
    company-jedi
    company-quickhelp
    company-statistics
    crux
    diff-hl
    dired+
    emojify
    eshell-prompt-extras
    exec-path-from-shell
    expand-region
    fill-column-indicator
    flx
    flx-ido
    flycheck
    gnuplot
    helm
    helm-ag
    helm-c-yasnippet
    helm-descbinds
    helm-flx
    helm-swoop
    highlight-symbol
    htmlize
    ibuffer-vc
    ido-completing-read+
    ido-hacks
    idomenu
    ido-ubiquitous
    ido-vertical-mode
    irony
    jedi-core
    less-css-mode
    lua-mode
    magit
    markdown-mode
    matlab-mode
    mmm-mode
    monokai-theme
    multiple-cursors
    multi-term
    neotree
    nlinum
    org-plus-contrib
    org-bullets
    page-break-lines
    powerline
    projectile
    pyvenv
    rainbow-delimiters
    sass-mode
    scss-mode
    session
    shell-pop
    smartparens
    toc-org
    undo-tree
    use-package
    vimrc-mode
    visual-regexp
    web-mode
    window-numbering
    which-key
    xterm-color
    yaml-mode
    yasnippet)
  "A list of packages to ensure are installed at launch.")

;; Check if all packages in `fx-packages' are installed
(defvar fx-packages-all-installed-p t
  "Non-nil means all packages in `fx-packages' are installed.")

(defun fx-packages-check-all-installed ()
  "Check if all packages needed installed."
  (dolist (pkg fx-packages)
    (unless (package-installed-p pkg)
     (setq fx-packages-all-installed-p nil))))

;; Install packages
(defun fx-install-packages ()
  "Install packages in `fx-packages'."
  (fx-packages-check-all-installed)
  (unless fx-packages-all-installed-p
    (package-refresh-contents)
    (dolist (pkg fx-packages)
      (unless (package-installed-p pkg)
        (package-install pkg)))
    (fx-packages-check-all-installed)
    (if fx-packages-all-installed-p
        (message "%s" "All packages in `fx-packages' are installed !")
      (message "%s" "Error occured installing packages :-("))))

;; Run package installation
(fx-install-packages)

(require 'use-package)

(provide 'init-elpa)
