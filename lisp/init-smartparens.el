;;;
(require 'smartparens)
(require 'smartparens-config)

;; global
(setq sp-autoskip-closing-pair 'always)
(setq sp-navigate-close-if-unbalanced t)
(smartparens-global-mode t)

;; highlights matching pairs
(show-smartparens-global-mode t)

;; Add smartparens-strict-mode to all sp--lisp-modes hooks. C-h v sp--lisp-modes
;; to customize/view this list.
;(mapc (lambda (mode)
;        (add-hook (intern (format "%s-hook" (symbol-name mode))) 'smartparens-strict-mode))
;      sp--lisp-modes)

(defun ome-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (previous-line)
  (indent-according-to-mode)
  (forward-line)
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

;; keybinding management
(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

(define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
(define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
(define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
(define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)

(define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
;(define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
(define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
(define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

(define-key sp-keymap (kbd "M-s n") 'sp-next-sexp)
(define-key sp-keymap (kbd "M-s p") 'sp-previous-sexp)

(define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
(define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)

(define-key sp-keymap (kbd "M-S-<delete>") 'sp-unwrap-sexp)
(define-key sp-keymap (kbd "M-S-<backspace>") 'sp-backward-unwrap-sexp)

(define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

(define-key sp-keymap (kbd "M-s M-i") 'sp-splice-sexp)
(define-key sp-keymap (kbd "M-s <down>") 'sp-splice-sexp-killing-forward)
(define-key sp-keymap (kbd "M-s <up>") 'sp-splice-sexp-killing-backward)
(define-key sp-keymap (kbd "M-s M-<backspace>") 'sp-splice-sexp-killing-around)

(define-key sp-keymap (kbd "M-s [") 'sp-select-previous-thing)
(define-key sp-keymap (kbd "M-s ]") 'sp-select-next-thing)

(define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
(define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)

(define-key sp-keymap (kbd "M-s M-t") 'sp-prefix-tag-object)
(define-key sp-keymap (kbd "M-s M-p") 'sp-prefix-pair-object)
(define-key sp-keymap (kbd "M-s M-c") 'sp-convolute-sexp)
(define-key sp-keymap (kbd "M-s M-a") 'sp-absorb-sexp)
(define-key sp-keymap (kbd "M-s M-e") 'sp-emit-sexp)
(define-key sp-keymap (kbd "M-s M-p") 'sp-add-to-previous-sexp)
(define-key sp-keymap (kbd "M-s M-n") 'sp-add-to-next-sexp)
(define-key sp-keymap (kbd "M-s M-j") 'sp-join-sexp)
(define-key sp-keymap (kbd "M-s M-s") 'sp-split-sexp)

;; pair management

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

;; tex-mode latex-mode
(sp-with-modes '(tex-mode plain-tex-mode latex-mode LaTeX-mode)
  (sp-local-tag "i" "\"<" "\">"))

;; markdown-mode
(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*" :bind "C-*")
  (sp-local-tag "2" "**" "**")
  (sp-local-tag "s" "```scheme" "```")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

;; html-mode
(sp-with-modes '(html-mode sgml-mode)
  (sp-local-pair "<" ">"))

;; lisp modes
(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil :bind "C-("))

;; wrapping
(sp-pair "(" ")" :wrap "C-(")

(dolist (mode '(c-mode c++-mode java-mode js2-mode sh-mode))
  (sp-local-pair mode
		 "{"
		 nil
		 :post-handlers
		 '((ome-create-newline-and-enter-sexp "RET"))))


(provide 'init-smartparens)
