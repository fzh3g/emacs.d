;;; init-latex.el --- Emacs configuration for LaTeX
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
    (setq LaTeX-indent-level 4)
    (setq TeX-newline-function 'newline-and-indent)
    (setq TeX-engine 'xetex)
    (setq TeX-auto-untabify t
          TeX-master nil
          TeX-show-compilation t
          TeX-auto-save t
          TeX-parse-self t
          LaTeX-syntactic-comments t
          TeX-save-query nil
          reftex-plug-into-AUCTeX t)
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
                (LaTeX-math-mode t)
                (flycheck-mode -1)
                (reftex-mode t)))

    (add-hook 'TeX-mode-hook
              (lambda ()
                (outline-minor-mode t)
                (flyspell-mode t)
                (TeX-interactive-mode t)
                (TeX-PDF-mode t)
                (TeX-fold-mode t)
                (visual-line-mode t)))))

(provide 'init-latex)
;;; init-latex.el ends here
