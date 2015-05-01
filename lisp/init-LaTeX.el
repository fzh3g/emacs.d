(setq LaTeX-indent-level 4)
(setq-default TeX-master nil)
(setq-default TeX-engine 'xetex)
(setq TeX-auto-untabify t
      TeX-show-compilation t
      TeX-auto-save t
      TeX-parse-self t
      LaTeX-syntactic-comments t
      TeX-save-query nil
      reftex-plug-into-AUCTeX t)

(setq TeX-view-program-selection
       '((output-pdf "Evince")
         (output-dvi "Evince")))

(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
(add-hook 'latex-mode-hook 'turn-on-cdlatex)

(add-hook 'LaTeX-mode-hook
	  (lambda ()
            (imenu-add-menubar-index)
            (LaTeX-math-mode t)
	    (reftex-mode t)))
(add-hook 'TeX-mode-hook
	  (lambda ()
	    (outline-minor-mode t)
	    (flyspell-mode t)
	    (TeX-interactive-mode t)
	    (TeX-PDF-mode t)
	    (TeX-fold-mode t)
	    (visual-line-mode t)))

(provide 'init-LaTeX)