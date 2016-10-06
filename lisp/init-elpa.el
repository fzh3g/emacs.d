;;; init-elpa.el --- Emacs configuration for package.el
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

;; Some configuration for installing packages.

;;; Code:

(require 'package)

;; Standard package repositories
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

;; Uncomment L22-23 and comment L26-29 if you find it slow
(setq package-archives
      '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("org"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

;; Fire up package.el
(package-initialize)
(setq package-enable-at-startup nil)

;; list of installed packages
(defvar fx-packages
  '(adaptive-wrap
    anzu
    auctex
    auctex-latexmk
    auto-compile
    avy
    buffer-move
    cmake-mode
    company
    company-auctex
    company-emoji
    company-flx
    company-irony
    company-irony-c-headers
    company-jedi
    company-quickhelp
    company-shell
    company-statistics
    company-web
    counsel
    crux
    ctags-update
    diminish
    dired+
    eshell-prompt-extras
    etags-select
    expand-region
    flx
    flycheck
    flycheck-irony
    flyspell-correct-ivy
    fullframe
    git-gutter+
    git-gutter-fringe+
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
    org-pomodoro
    page-break-lines
    powerline
    projectile
    pyvenv
    rainbow-delimiters
    rainbow-mode
    sass-mode
    scss-mode
    session
    shell-pop
    smartparens
    swiper
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

(defvar fx-theme-packages
  '(dracula-theme
    leuven-theme
    monokai-theme
    color-theme-sanityinc-tomorrow
    solarized-theme
    zenburn-theme)
  "A list of theme packages.")

(setq fx-packages (append fx-packages fx-theme-packages))

(unless *linux*
  (add-to-list 'fx-packages 'exec-path-from-shell))

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

(provide 'init-elpa)
;;; init-elpa.el ends here
