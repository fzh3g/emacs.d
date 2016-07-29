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

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/General-VC-Options.html
(setq vc-follow-symlinks t)

;; disable overwrite mode
(put 'overwrite-mode 'disabled t)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(define-key global-map(kbd "RET") 'newline-and-indent)

;; join line
(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-c J") #'(lambda () (interactive) (join-line 1)))

;; Change "yes or no" to "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;; doc view
(setq doc-view-continuous t)

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

;; taken from Prelude
(defmacro fx|advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.
The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command
                      (,class ,(intern (concat
                                        (symbol-name command)
                                        "-"
                                        advice-name))
                              activate)
                    ,@body))
               commands)))

(defvar fx-indent-sensitive-modes
  '(coffee-mode
    elm-mode
    haml-mode
    haskell-mode
    slim-mode
    makefile-mode
    makefile-bsdmake-mode
    makefile-gmake-mode
    makefile-imake-mode
    python-mode)
  "Modes for which auto-indenting is suppressed.")

(defcustom fx-yank-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur."
  :type 'number
  :group 'fx)

(defcustom fx-yank-indent-modes '(LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here."
  :type 'list
  :group 'fx)

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode fx-indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (indent-region (point-min) (point-max))
          (message "Indented buffer.")))
      (whitespace-cleanup))))
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

;; indent on paste
(defun fx/yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) fx-yank-indent-threshold)
      (indent-region beg end nil)))

(fx|advise-commands
 "indent" (yank yank-pop) after
 "If current mode is one of `fx-yank-indent-modes',
indent yanked text (with prefix arg don't indent)."
 (if (and (not (ad-get-arg 0))
          (not (member major-mode fx-indent-sensitive-modes))
          (or (derived-mode-p 'prog-mode)
              (member major-mode fx-yank-indent-modes)))
     (let ((transient-mark-mode nil))
       (fx/yank-advised-indent-function (region-beginning) (region-end)))))

(defun fx/dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))
(defun fx/unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))
(global-set-key (kbd "C-x f") nil)
(global-set-key (kbd "C-x f c u") #'fx/dos2unix)
(global-set-key (kbd "C-x f c d") #'fx/unix2dos)

;; http://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/
(defun fx/show-and-copy-buffer-filename ()
  "Show the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new file-name))
      (error "Buffer not visiting a file"))))
(global-set-key (kbd "C-x f y") #'fx/show-and-copy-buffer-filename)

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

(use-package ediff
  :defer t
  :init
  (progn
    (setq-default
     ediff-window-setup-function 'ediff-setup-windows-plain
     ;; emacs is evil and decrees that vertical shall henceforth be horizontal
     ediff-split-window-function 'split-window-horizontally
     ediff-merge-split-window-function 'split-window-horizontally)
    (add-hook 'ediff-quit-hook #'winner-undo)))

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
  :init
  (progn
    (defalias 'redo 'undo-tree-redo)
    (defalias 'undo 'undo-tree-undo)
    (global-undo-tree-mode))
  :config
  (progn
    (setq undo-tree-auto-save-history t)
    (let ((undo-dir (concat fx-cache-directory "undo/")))
      (setq undo-tree-history-directory-alist
            `(("." . ,undo-dir)))
      (unless (file-exists-p undo-dir)
        (make-directory undo-dir t)))))

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
  (("M-s s" . avy-goto-char)
   ("M-s w" . avy-goto-word-or-subword-1)
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
  (setq mc/list-file (concat fx-cache-directory "mc-lists.el")))

(use-package crux
  :init
  (progn
    (global-set-key [remap move-beginning-of-line]
                    #'crux-move-beginning-of-line)
    (global-set-key [remap kill-whole-line] #'crux-kill-whole-line)
    (global-set-key (kbd "C-x f o") #'crux-open-with)
    (global-set-key (kbd "C-o") #'crux-smart-open-line)
    (global-set-key (kbd "C-S-o") #'crux-smart-open-line-above)
    (global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)
    (global-set-key (kbd "C-x f r") #'crux-rename-file-and-buffer)
    (global-set-key (kbd "C-x f d") #'crux-delete-file-and-buffer)
    (global-set-key (kbd "C-x f s") #'crux-sudo-edit)
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
