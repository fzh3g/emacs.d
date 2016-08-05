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
      compilation-scroll-output 'first-error
      set-mark-command-repeat-pop t
      delete-selection-mode t
      kill-whole-line t)

;; Hack to fix a bug with tabulated-list.el
;; see: http://redd.it/2dgy52
(defun tabulated-list-revert (&rest ignored)
  "The `revert-buffer-function' for `tabulated-list-mode'.
It runs `tabulated-list-revert-hook', then calls `tabulated-list-print'."
  (interactive)
  (unless (derived-mode-p 'tabulated-list-mode)
    (error "The current buffer is not in Tabulated List mode"))
  (run-hooks 'tabulated-list-revert-hook)
  ;; hack is here
  (tabulated-list-print))

;; Mouse cursor in terminal mode
(xterm-mouse-mode 1)

;; disable C-<down-mouse-1>
(global-set-key (kbd "C-<down-mouse-1>") nil)

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/General-VC-Options.html
(setq vc-follow-symlinks t)

;; persistent abbreviation file
(setq abbrev-file-name (concat fx-cache-directory "abbrev_defs"))

;; Text
(setq longlines-show-hard-newlines t)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Single space between sentences is more widespread than double
(setq-default sentence-end-double-space nil)

;; disable overwrite mode
(put 'overwrite-mode 'disabled t)

;; seems pointless to warn. There's always undo.
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(which-key-declare-prefixes "C-x n" "narrow")

(global-set-key (kbd "C-x f") nil)
(global-set-key (kbd "C-x f e") 'erase-buffer)

(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)

;; join line
(defun fx/join-line-below ()
  "Join line below."
  (interactive)
  (join-line 1))
(global-set-key (kbd "C-c j L") #'fx/join-line-below)
(global-set-key (kbd "C-c j l") 'join-line)
(which-key-declare-prefixes "\C-c j" "join line")

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

;; Auto refresh
(global-auto-revert-mode t)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; Switch between unix and dos format
(defun fx/dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))
(defun fx/unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))
(global-set-key (kbd "C-x f c u") #'fx/dos2unix)
(global-set-key (kbd "C-x f c d") #'fx/unix2dos)
(which-key-declare-prefixes "C-x f c" "unix dos")


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
  :defer t
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

(use-package highlight-numbers
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook 'highlight-numbers-mode)
    (add-hook 'asm-mode-hook (lambda () (highlight-numbers-mode -1)))))

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
    (which-key-mode))
  :config
  (progn
    (which-key-declare-prefixes "C-x RET" "coding-system")
    (which-key-declare-prefixes "C-x r" "rectangle&register")
    (which-key-declare-prefixes "C-x a" "abbrev")
    (which-key-declare-prefixes "C-x 8" "special character")
    (which-key-declare-prefixes "C-x @" "event modifier")))

;; anzu
(use-package anzu
  :defer t
  :init (global-anzu-mode t)
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (progn
    (setq anzu-search-threshold 1000
          anzu-replace-to-string-separator " => "
          anzu-cons-mode-line-p nil
          anzu-deactivate-region t
          anzu-mode-lighter "")))

(use-package visual-regexp
  :init (which-key-declare-prefixes "C-c v" "visual regexp")
  :bind (("C-c v r" . vr/replace)
         ("C-c v q" . vr/query-replace)
         ("C-c v m" . vr/mc-mark)))

;; avy
(use-package avy
  :config
  (progn
    (defun fx/avy-goto-url()
      "Use avy to go to an URL in the buffer."
      (interactive)
      (avy--generic-jump "https?://" nil 'pre))
    (defun fx/avy-open-url ()
      "Use avy to select an URL in the buffer and open it."
      (interactive)
      (save-excursion
        (fx/avy-goto-url)
        (browse-url-at-point))))
  :bind
  (("M-s SPC" . avy-goto-word-or-subword-1)
   ("M-s s" . avy-goto-char)
   ("M-s l" . avy-goto-line)
   ("M-s m" . avy-pop-mark)
   ("M-s o" . fx/avy-open-url)))

(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)
   ("C-S-<mouse-1>" . mc/add-cursor-on-click)
   ("C-c m r" . set-rectangular-region-anchor)
   ("C-c m c" . mc/edit-lines)
   ("C-c m e" . mc/edit-ends-of-lines)
   ("C-c m a" . mc/edit-beginnings-of-lines))
  :init
  (progn
    (which-key-declare-prefixes "C-c m" "multiple cursors")
    (setq mc/list-file (concat fx-cache-directory "mc-lists.el"))))

(use-package crux
  :init (which-key-declare-prefixes "C-x f" "crux&files")
  :bind
  (([remap move-beginning-of-line] . crux-move-beginning-of-line)
   ([remap kill-whole-line] . crux-kill-whole-line)
   ("C-x f o" . crux-open-with)
   ("C-o" . crux-smart-open-line)
   ("C-S-o" . crux-smart-open-line-above)
   ("C-<backspace>" . crux-kill-line-backwards)
   ("C-x f r" . crux-rename-file-and-buffer)
   ("C-x f d" . crux-delete-file-and-buffer)
   ("C-x f s" . crux-sudo-edit)))

(use-package hungry-delete
  :diminish hungry-delete-mode
  :defer t
  :init (global-hungry-delete-mode)
  :config
  (progn
    (setq-default hungry-delete-chars-to-skip " \t\f\v")
    (define-key hungry-delete-mode-map (kbd "DEL") 'hungry-delete-backward)
    (define-key hungry-delete-mode-map (kbd "S-DEL") 'delete-backward-char)))

(use-package adaptive-wrap
  :defer t
  :init
  (progn
    (add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)))

(use-package iedit
  :bind
  (("C-;" . iedit-mode)
   ("C-h C-;" . iedit-mode-toggle-on-function)
   ("C-x r <return>" . iedit-rectangle-mode)))

(use-package lorem-ipsum
  :bind
  (("C-c l s" . lorem-ipsum-insert-sentences)
   ("C-c l p" . lorem-ipsum-insert-paragraphs)
   ("C-c l l" . lorem-ipsum-insert-list))
  :init
  (which-key-declare-prefixes "\C-c l" "lorem ipsum"))

(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
