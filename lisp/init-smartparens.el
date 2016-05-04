(use-package smartparens
  :defer t
  :init
  (progn
    (setq sp-show-pair-delay 0.2
          sp-skip-closing-pair 'always
          sp-navigate-close-if-unbalanced t
          sp-show-pair-from-inside t
          sp-cancel-autoskip-on-backward-movement nil)
    (show-smartparens-global-mode t)
    (smartparens-global-mode t))
  :config
  (progn
    (require 'smartparens-config)

    ;; keybinding management
    (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
    (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

    (define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
    (define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)

    (define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
    (define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)

    (define-key sp-keymap (kbd "C-M-u") 'sp-up-sexp)
    (define-key sp-keymap (kbd "C-M-e") 'sp-backward-up-sexp)

    (define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
    (define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

    (define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

    (define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
    (define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)

    (define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
    (define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
    (define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
    (define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

    (define-key sp-keymap (kbd "M-s <right>") 'sp-splice-sexp-killing-forward)
    (define-key sp-keymap (kbd "M-s <left>") 'sp-splice-sexp-killing-backward)
    (define-key sp-keymap (kbd "M-s <backspace>") 'sp-splice-sexp-killing-around)
    (define-key sp-keymap (kbd "M-S-<delete>") 'sp-unwrap-sexp)
    (define-key sp-keymap (kbd "M-S-<backspace>") 'sp-backward-unwrap-sexp)

    (define-key sp-keymap (kbd "M-s M-r") 'sp-rewrap-sexp)

    (define-key sp-keymap (kbd "M-s j") 'sp-newline)

    (define-key sp-keymap (kbd "M-s M-i") 'sp-splice-sexp)
    (define-key sp-keymap (kbd "M-s M-j") 'sp-join-sexp)
    (define-key sp-keymap (kbd "M-s M-s") 'sp-split-sexp)

    (define-key sp-keymap (kbd "M-s M-p") 'sp-add-to-previous-sexp)
    (define-key sp-keymap (kbd "M-s M-n") 'sp-add-to-next-sexp)

    (define-key sp-keymap (kbd "M-s M-t") 'sp-prefix-tag-object)
    (define-key sp-keymap (kbd "M-s M-p") 'sp-prefix-pair-object)
    (define-key sp-keymap (kbd "M-s M-c") 'sp-convolute-sexp)
    (define-key sp-keymap (kbd "M-s M-a") 'sp-absorb-sexp)
    (define-key sp-keymap (kbd "M-s M-e") 'sp-emit-sexp)

    (define-key sp-keymap (kbd "M-s [") 'sp-select-previous-thing)
    (define-key sp-keymap (kbd "M-s ]") 'sp-select-next-thing)

    (define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
    (define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)

    (bind-key ";" 'sp-comment emacs-lisp-mode-map)

    ;; pair management

    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
    (sp-pair "(" ")" :wrap "C-(")

    ;; tex-mode latex-mode
    (sp-with-modes '(tex-mode plain-tex-mode latex-mode LaTeX-mode)
      (sp-local-tag "i" "\"<" "\">"))

    ;; markdown-mode
    (defun sp--gfm-skip-asterisk (ms mb me)
      (save-excursion
        (goto-char mb)
        (save-match-data (looking-at "^\\* "))))
    (sp-with-modes '(markdown-mode gfm-mode rst-mode)
      (sp-local-pair "*" "*"
                     :wrap "C-*"
                     :unless '(sp-point-after-word-p sp-point-at-bol-p)
                     :post-handlers '(("[d1]" "SPC"))
                     :skip-match 'sp--gfm-skip-asterisk)
      (sp-local-pair "**" "**")
      (sp-local-pair "_" "_" :wrap "C-_" :unless '(sp-point-after-word-p)))

    ;; org-mode
    (defun sp--org-skip-asterisk (ms mb me)
      (or (and (= (line-beginning-position) mb)
               (eq 32 (char-after (1+ mb))))
          (and (= (1+ (line-beginning-position)) me)
               (eq 32 (char-after me)))))
    (sp-with-modes 'org-mode
      (sp-local-pair "*" "*"
                     :actions '(insert wrap)
                     :unless '(sp-point-after-word-p sp-point-at-bol-p)
                     :wrap "C-*"
                     :post-handlers '(("[d1]" "SPC"))
                     :skip-match 'sp--org-skip-asterisk)
      (sp-local-pair "_" "_" :unless '(sp-point-after-word-p) :wrap "C-_")
      (sp-local-pair "/" "/"
                     :unless '(sp-point-after-word-p)
                     :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "~" "~"
                     :unless '(sp-point-after-word-p)
                     :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "=" "="
                     :unless '(sp-point-after-word-p)
                     :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "«" "»"))

    ;; lisp modes
    (defun my-add-space-after-sexp-insertion (id action _context)
      (when (eq action 'insert)
        (save-excursion
          (forward-char (sp-get-pair id :cl-l))
          (when (or (eq (char-syntax (following-char)) ?w)
                    (looking-at (sp--get-opening-regexp)))
            (insert " ")))))

    (defun my-add-space-before-sexp-insertion (id action _context)
      (when (eq action 'insert)
        (save-excursion
          (backward-char (length id))
          (when (or (eq (char-syntax (preceding-char)) ?w)
                    (and (looking-back (sp--get-closing-regexp))
                         (not (eq (char-syntax (preceding-char)) ?'))))
            (insert " ")))))
    (sp-with-modes sp--lisp-modes
      (sp-local-pair "(" nil
                     :wrap "C-("
                     :pre-handlers '(my-add-space-before-sexp-insertion)
                     :post-handlers '(my-add-space-after-sexp-insertion)))

    ;; C++
    (sp-with-modes '(c-mode c++-mode java-mode js2-mode sh-mode)
      (sp-local-pair "{" "}" :post-handlers '(("||\n[i]" "RET"))))
    (sp-local-pair 'c++-mode "/*" "*/" :post-handlers '((" | " "SPC")
                                                        ("* ||\n[i]" "RET")))
))

(provide 'init-smartparens)
