;;; init-latex.el --- Emacs configuration for LaTeX
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

;; Some configuration for LaTeX.

;;; Code:

(use-package tex
  :defer t
  :init
  (progn
    ;; indentation
    (setq LaTeX-indent-level 4)
    (setq TeX-newline-function 'newline-and-indent)
    ;; use XeLaTeX
    (setq TeX-engine 'xetex)
    ;; some variables
    (setq TeX-auto-untabify t
          TeX-master nil
          TeX-show-compilation nil
          TeX-auto-save t
          TeX-parse-self t
          LaTeX-syntactic-comments t
          ;; reftex
          reftex-plug-into-AUCTeX '(nil nil t t t)
          reftex-use-fonts t
          ;; Don't insert line-break at inline math
          LaTeX-fill-break-at-separators nil)
    ;; for Minted
    (setq TeX-command-extra-options "-shell-escape")

    ;; default viewer
    (when *linux*
      (cond
       ((executable-find "okular")
        (setq TeX-view-program-selection
              '((output-pdf "Okular")
                (output-dvi "Okular"))))
       ((executable-find "evince")
        (setq TeX-view-program-selection
              '((output-pdf "Evince")
                (output-dvi "Evince"))))
       (t
        (setq TeX-view-program-selection
              '((output-pdf "xdg-open")
                (output-dvi "xdg-open"))))))

    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (imenu-add-menubar-index)
                (outline-minor-mode t)
                (LaTeX-math-mode t)
                (flycheck-mode -1)
                (reftex-mode t)
                (TeX-fold-mode t)
                (TeX-interactive-mode t)
                (TeX-PDF-mode t)
                (auto-fill-mode))))
  :config
  (progn
    ;; smartparens
    (require 'smartparens-latex)))

(use-package latex-extra
  :diminish latex-extra-mode
  :defer t
  :init
  (progn
    (add-hook 'LaTeX-mode-hook #'latex-extra-mode))
  :config
  (progn
    (define-key latex-extra-mode-map (kbd "C-M-a") nil)
    (define-key latex-extra-mode-map (kbd "C-M-e") nil)
    (define-key latex-extra-mode-map (kbd "C-M-f") nil)
    (define-key latex-extra-mode-map (kbd "C-M-b") nil)))

(use-package auctex-latexmk
  :defer t
  :init
  (progn
    (setq auctex-latexmk-inherit-TeX-PDF-mode nil)
    (add-hook 'LaTeX-mode-hook #'auctex-latexmk-setup)))

(provide 'init-latex)
;;; init-latex.el ends here
