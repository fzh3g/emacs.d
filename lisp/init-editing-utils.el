;; some basic preferences
(setq-default mouse-yank-at-point t
              buffers-menu-max-size 30
              case-fold-search t
              compilation-scroll-output t
              set-mark-command-repeat-pop t
              visible-bell t
              delete-selection-mode t)

; http://www.gnu.org/software/emacs/manual/html_node/emacs/General-VC-Options.html
(setq vc-follow-symlinks t)

;; disable overwrite mode
(put 'overwrite-mode 'disabled t)

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

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq tab-width 4)

(define-key global-map(kbd "RET") 'newline-and-indent)
(defun my:newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
(global-set-key (kbd "S-<return>") 'my:newline-at-end-of-line)

;; show column number and line number
(use-package nlinum
  :init (setq-default linum-delay t)
  :config
  (dolist (hook '(prog-mode-hook
                  conf-mode-hook
                  yaml-mode-hook
                  web-mode-hook
                  markdown-mode-hook
                  matlab-mode-hook
                  css-mode-hook))
    (add-hook hook 'column-number-mode)
    (add-hook hook 'line-number-mode)
    (add-hook hook 'nlinum-mode)))

;; fill column indicator
(use-package fill-column-indicator
  :init
  (progn
    (setq fci-rule-width 1)
    (setq fci-rule-column 80)
    ;; (setq fci-rule-color "dimgray")

    (dolist (hook '(prog-mode-hook markdown-mode-hook))
      (add-hook hook 'fci-mode))

    (defun fx/auto-fci-mode (&optional unused)
      "Automatically turn off fci-mode when window is too narrow"
      (if (< (window-width) fci-rule-column)
          (fci-mode -1)
        (fci-mode 1)))

    (add-hook 'after-change-major-mode-hook 'fx/auto-fci-mode)
    (add-hook 'window-configuration-change-hook 'fx/auto-fci-mode)))

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; kill whole line
(setq kill-whole-line t)

;; nice scrolling
(setq scroll-margin 5
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      auto-window-vscroll nil)

;; Change "yes or no" to "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

(global-auto-revert-mode t)
(setq-default global-auto-revert-non-file-buffers t
              auto-revert-verbose nil)

(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode))

;; image-dired
(setq-default image-dired-dir (concat fx-cache-directory "image-dired/")
              image-dired-gallery-dir (concat image-dired-dir ".image-dired_gallery")
              image-dired-db-file (concat image-dired-dir ".image-dired_db")
              image-dired-temp-image-file (concat image-dired-dir ".image-dired_temp")
              image-dired-temp-rotate-image-file (concat image-dired-dir ".image-dired_rotate_temp"))

;; doc view
(setq-default doc-view-continuous t)

;; page break lines
(use-package page-break-lines
  :init
  (global-page-break-lines-mode))

(use-package expand-region
  :bind
  (("C-}" . er/expand-region)
   ("C-{" . er/contract-region)))

(use-package undo-tree
  :init (global-undo-tree-mode))

(use-package neotree
  :defer t
  :bind (("<f8>" . neotree-toggle))
  :config (setq neo-window-width 28
                neo-create-file-auto-open t
                neo-banner-message nil
                neo-show-updir-line nil
                neo-mode-line-type 'neotree
                neo-smart-open t
                neo-dont-be-alone t
                neo-persist-show nil
                neo-show-hidden-files t
                neo-auto-indent-point t))

(use-package highlight-symbol
  :defer t
  :init
  (dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
    (add-hook hook 'highlight-symbol-mode)
    (add-hook hook 'highlight-symbol-nav-mode)))

(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package which-key
  :init
  (progn
    (setq which-key-use-C-h-for-paging t
          which-key-prevent-C-h-from-cycling t
          which-key-sort-order 'which-key-key-order-alpha
          which-key-popup-type 'side-window
          which-key-side-window-location '(right bottom)
          which-key-max-description-length 23
          which-key-side-window-max-width 0.5
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
  (("C-:" . avy-goto-char)
   ("C-'" . avy-goto-line)
   ("M-g f" . avy-goto-char-2)
   ("M-g w" . avy-goto-subword-1)))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         ("C-S-c r" . set-rectangular-region-anchor)
         ("C-S-c c" . mc/edit-lines)
         ("C-S-c e" . mc/edit-ends-of-lines)
         ("C-S-c a" . mc/edit-beginnings-of-lines))
  :init
  (setq mc/list-file (concat fx-cache-directory ".mc-lists.el")))

;; join line
(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-c J") (lambda () (interactive) (join-line 1)))

;; Move more quickly
(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-line 5))))

(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-line -5))))

(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))

;; move to beginning of line, thanks to Prelude
(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key (kbd "C-a") 'prelude-move-beginning-of-line)


(provide 'init-editing-utils)
