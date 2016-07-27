;;; init-editing-utils.el --- Emacs configuration for editing
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

;; Some configuration for better editing experience.

;;; Code:

;; some basic preferences
(setq mouse-yank-at-point t
      buffers-menu-max-size 30
      case-fold-search t
      compilation-scroll-output t
      set-mark-command-repeat-pop t
      visible-bell t
      delete-selection-mode t
      kill-whole-line t)

;; cursor don't blink
(blink-cursor-mode -1)

; https://www.gnu.org/software/emacs/manual/html_node/emacs/General-VC-Options.html
(setq vc-follow-symlinks t)

;; disable overwrite mode
(put 'overwrite-mode 'disabled t)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(define-key global-map(kbd "RET") 'newline-and-indent)

;; http://emacswiki.org/emacs/RevertBuffer
(global-set-key
  (kbd "<f5>")
  (lambda (&optional force-reverting)
    "Interactive call to revert-buffer. Ignoring the auto-save
 file and not requesting for confirmation. When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
    (interactive "P")
    ;;(message "force-reverting value is %s" force-reverting)
    (if (or force-reverting (not (buffer-modified-p)))
        (revert-buffer :ignore-auto :noconfirm)
      (error "The buffer has been modified"))))

(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; join line
(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-c J") #'(lambda () (interactive) (join-line 1)))

;; Change "yes or no" to "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;; doc view
(setq doc-view-continuous t)

;; fill column indicator
(use-package fill-column-indicator
  :init
  (progn
    (setq fci-rule-width 1)
    ;; (setq fci-rule-column 80)
    ;; (setq fci-rule-color "dimgray")
    (dolist (hook '(prog-mode-hook
                    markdown-mode-hook
                    git-commit-mode-hook))
      (add-hook hook 'fci-mode))
    ;; Regenerate fci-mode line images after switching themes
    (defun sanityinc/fci-enabled-p ()
      (bound-and-true-p fci-mode))
    (defadvice enable-theme (after recompute-fci-face activate)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (sanityinc/fci-enabled-p)
            (turn-on-fci-mode)))))))

;; page break lines
(use-package page-break-lines
  :diminish page-break-lines-mode
  :init
  (global-page-break-lines-mode))

(use-package expand-region
  :bind
  (("C-M-]" . er/expand-region)
   ("C-M-[" . er/contract-region)))

(use-package undo-tree
  :diminish undo-tree-mode
  :init (global-undo-tree-mode))

(use-package highlight-symbol
  :diminish hi-lock-mode
  :diminish highlight-symbol-mode
  :defer t
  :init
  (dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
    (add-hook hook 'highlight-symbol-mode)
    (add-hook hook 'highlight-symbol-nav-mode))
  :bind
  ("M-s r" . highlight-symbol-query-replace))

(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package which-key
  :diminish which-key-mode
  :init
  (progn
    (setq which-key-use-C-h-for-paging t
          which-key-prevent-C-h-from-cycling t
          which-key-sort-order 'which-key-key-order-alpha
          which-key-popup-type 'side-window
          which-key-side-window-location 'bottom
          which-key-max-description-length 26
          which-key-side-window-max-height 0.4
          which-key-special-keys nil)
    (which-key-mode)))

;; anzu
(use-package anzu
  :defer t
  :init (global-anzu-mode t)
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (custom-set-variables
   '(anzu-mode-lighter "")
   '(anzu-deactivate-region t)
   '(anzu-search-threshold 1000)
   '(anzu-replace-to-string-separator " => ")))

;; avy
(use-package avy
  :bind
  (("M-s w" . avy-goto-word-or-subword-1)
   ("M-s l" . avy-goto-line)))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         ("C-c m r" . set-rectangular-region-anchor)
         ("C-c m c" . mc/edit-lines)
         ("C-c m e" . mc/edit-ends-of-lines)
         ("C-c m a" . mc/edit-beginnings-of-lines))
  :init
  (setq mc/list-file (concat fx-cache-directory ".mc-lists.el")))

(use-package crux
  :init
  (progn
    (global-set-key [remap move-beginning-of-line]
                    #'crux-move-beginning-of-line)
    (global-set-key [remap kill-whole-line] #'crux-kill-whole-line)
    (global-set-key (kbd "C-c o") #'crux-open-with)
    (global-set-key (kbd "C-o") #'crux-smart-open-line)
    (global-set-key (kbd "C-S-o") #'crux-smart-open-line-above)
    (global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)
    (global-set-key (kbd "C-c r") #'crux-rename-file-and-buffer)
    ))

(use-package hungry-delete
  :diminish hungry-delete-mode
  :defer t
  :init (global-hungry-delete-mode)
  :config
  (progn
    (setq-default hungry-delete-chars-to-skip " \t\f\v")
    (define-key hungry-delete-mode-map (kbd "DEL") 'hungry-delete-backward)
    (define-key hungry-delete-mode-map (kbd "S-DEL") 'delete-backward-char)))

(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
