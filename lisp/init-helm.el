(require 'helm)
(require 'helm-ag)
(require 'helm-config)

;(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
;(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;(define-key helm-map (kbd "C-z")  'helm-select-action)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h x") 'helm-register)
(global-set-key (kbd "C-c h M-:") 'helm-eval-expression-with-eldoc)
(global-set-key (kbd "C-x C-o") 'helm-find-files)
(global-set-key (kbd "C-c h g") 'helm-do-grep)
(global-set-key (kbd "M-g .") 'helm-do-ag)
(global-set-key (kbd "C-h a") 'helm-apropos)
(global-set-key (kbd "C-c h C-c w") 'helm-wikipedia-suggest)

;; shell history.
;(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)
(add-hook 'shell-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-l") 'helm-comint-input-ring)))
(add-hook 'inferior-python-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-l") 'helm-comint-input-ring)))
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "C-c C-l") 'helm-eshell-history)))
;; minibuffer history
;(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p t
      helm-move-to-line-cycle-in-source t
      helm-ff-search-library-in-sexp t
      helm-scroll-amount 8
      helm-candidate-number-limit 500
      helm-ff-file-name-history-use-recentf t)

(setq helm-M-x-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t
      helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match t
      helm-locate-fuzzy-match t
      helm-apropos-fuzzy-match t
      helm-ag-fuzzy-match t
      helm-lisp-fuzzy-completion t)

(setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
(setq helm-ag-insert-at-point 'symbol)

(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)


(helm-mode 1)
(helm-autoresize-mode t)
(setq helm-autoresize-max-height 50)

(require 'helm-gtags)
(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t)

(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'python-mode-hook 'helm-gtags-mode)
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)

(add-hook 'helm-gtags-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-j") 'helm-gtags-select)
             (local-set-key (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
             (local-set-key (kbd "C-c g t") 'helm-gtags-find-tag)
             (local-set-key (kbd "C-c g r") 'helm-gtags-find-rtag)
             (local-set-key (kbd "C-c g s") 'helm-gtags-find-symbol)
             (local-set-key (kbd "C-c g d") 'helm-gtags-dwim)
             (local-set-key (kbd "C-c g t") 'helm-gtags-pop-stack)))


;; helm swoop
(require 'helm-swoop)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)
;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows nil)
;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)
;; Go to the opposite side of line from the end or beginning of line
(setq helm-swoop-move-to-line-cycle t)
;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color t)


(provide 'init-helm)

