;;; init-elpa.el --- Emacs configuration for package.el
;; -*- coding: utf-8 -*-
;;
;; Copyright (c) 2015-2018 Faxiang Zheng
;;
;; Author: Faxiang Zheng <fxzheng0906@outlook.com>
;; URL: https://github.com/zhengfaxiang/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Some configuration for installing packages.

;;; Code:

(require 'package)

;; Standard package repositories
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

;; Uncomment L22-23 and comment L26-29 if you find it slow
(setq package-archives
      '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

;; Fire up package.el
(package-initialize)
(setq package-enable-at-startup nil)

;; list of installed packages
(defvar fx-packages
  '(anzu
    auctex
    auctex-latexmk
    auto-compile
    avy
    beacon
    buffer-move
    cmake-mode
    company
    company-auctex
    company-flx
    company-irony
    company-irony-c-headers
    company-jedi
    company-quickhelp
    company-shell
    company-statistics
    company-web
    counsel
    counsel-etags
    crux
    ctags-update
    diminish
    dired-narrow
    dired-subtree
    eshell-prompt-extras
    exec-path-from-shell
    expand-region
    flx
    flycheck
    flycheck-irony
    flyspell-correct-ivy
    fullframe
    git-gutter+
    git-gutter-fringe+
    goto-chg
    gnuplot
    google-this
    highlight-numbers
    highlight-symbol
    htmlize
    hungry-delete
    ibuffer-projectile
    iedit
    imenu-anywhere
    irony
    irony-eldoc
    ivy
    jedi-core
    latex-extra
    less-css-mode
    lorem-ipsum
    lua-mode
    magit
    markdown-mode
    matlab-mode
    mmm-mode
    multiple-cursors
    multi-term
    nlinum
    org-bullets
    org-plus-contrib
    page-break-lines
    powerline
    projectile
    pyvenv
    quelpa
    rainbow-delimiters
    rainbow-mode
    sass-mode
    scss-mode
    shell-pop
    smartparens
    smex
    spaceline
    swiper
    toc-org
    undo-tree
    use-package
    vimrc-mode
    visual-regexp
    web-beautify
    web-mode
    wgrep
    winum
    which-key
    writeroom-mode
    xterm-color
    yaml-mode
    yasnippet)
  "A list of packages to ensure are installed at launch.")

(defvar fx-theme-packages
  '(dracula-theme
    leuven-theme
    monokai-theme
    color-theme-sanityinc-tomorrow
    solarized-theme
    zenburn-theme
    gruvbox-theme)
  "A list of theme packages.")

(setq fx-packages (append fx-packages fx-theme-packages))

(defvar fx-packages-all-installed-p t
  "Non-nil means all packages in `fx-packages' are installed.")

(defun fx-packages-check-all-installed ()
  "Check if all packages needed installed."
  (dolist (pkg fx-packages)
    (unless (package-installed-p pkg)
      (setq fx-packages-all-installed-p nil))))

(defun fx-install-packages ()
  "Install packages in `fx-packages'."
  (fx-packages-check-all-installed)
  (unless fx-packages-all-installed-p
    (package-refresh-contents)
    (dolist (pkg fx-packages)
      (unless (package-installed-p pkg)
        (package-install pkg)))))

;; Run package installation
(fx-install-packages)

(require 'use-package)

;; quelpa
(use-package quelpa
  :config
  (setq quelpa-upgrade-p nil
        quelpa-self-upgrade-p nil
        quelpa-checkout-melpa-p nil
        quelpa-update-melpa-p nil
        quelpa-melpa-recipe-stores nil)
  )

;; dired+
(unless (package-installed-p 'dired+)
  (quelpa '(dired+ :fetcher wiki)))

(provide 'init-elpa)
;;; init-elpa.el ends here
