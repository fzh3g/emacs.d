;;; init-elpa.el --- Emacs configuration for package.el
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

;; Uncomment L21-22 and comment L25-28 if you find it slow
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
  '(adaptive-wrap
    anzu
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
    counsel
    crux
    diff-hl
    diminish
    dired+
    eshell-prompt-extras
    expand-region
    fill-column-indicator
    flycheck
    flycheck-irony
    flyspell-correct-ivy
    gnuplot
    google-this
    highlight-numbers
    highlight-symbol
    htmlize
    hungry-delete
    ibuffer-vc
    iedit
    imenu-anywhere
    irony
    irony-eldoc
    jedi-core
    latex-extra
    less-css-mode
    leuven-theme
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

(unless *linux*
  (add-to-list 'fx-packages 'exec-path-from-shell))

(unless *is-a-mac*
  (add-to-list 'fx-packages 'emojify))

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
      )))

;; Run package installation
(fx-install-packages)

(require 'use-package)

(provide 'init-elpa)
;;; init-elpa.el ends here
