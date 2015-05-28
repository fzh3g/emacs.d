;; some basic preferences
(setq-default mouse-yank-at-point t
              buffers-menu-max-size 30
              case-fold-search t
              compilation-scroll-output t
              set-mark-command-repeat-pop t
              visible-bell t
              delete-selection-mode t)

;; http://www.quora.com/Whats-the-best-way-to-edit-remote-files-from-Emacs
(setq-default tramp-default-method "ssh")
(setq-default tramp-auto-save-directory "~/.backups/tramp/")
(setq-default tramp-chunksize 8192)

; http://www.gnu.org/software/emacs/manual/html_node/emacs/General-VC-Options.html
(setq vc-follow-symlinks t)

;; sudo save
(defun sudo-save ()
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))
(global-set-key (kbd "C-x M-s") 'sudo-save)

(global-set-key (kbd "C-x M-r") 'toggle-read-only)

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
(setq-default python-indent 4)
(setq-default python-indent-offset 4)
(setq-default python-indent-guess-indent-offset nil)
(setq-default python-guess-indent nil)

(define-key global-map(kbd "RET") 'newline-and-indent)
(defun my:newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
(global-set-key (kbd "S-<return>") 'my:newline-at-end-of-line)

;; show column number and line number
;(global-linum-mode t)
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook 'linum-mode)
  (add-hook hook 'column-number-mode)
  (add-hook hook 'line-number-mode))
(add-hook 'linum-mode-hook
          (lambda ()
            (setq-default linum--width
                          (length (number-to-string
                                   (count-lines (point-min) (point-max)))))))

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; kill whole line
(setq kill-whole-line t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)


;; Change "yes or no" to "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

(global-auto-revert-mode t)
(setq-default global-auto-revert-non-file-buffers t
              auto-revert-verbose nil)


(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode))

;; doc view
(setq-default doc-view-continuous t)


(require 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)
(global-set-key (kbd "C-M-@") 'er/contract-region)

(require 'undo-tree)
(global-undo-tree-mode)

(require 'highlight-symbol)
(dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
  (add-hook hook 'highlight-symbol-mode)
  (add-hook hook 'highlight-symbol-nav-mode))

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'highlight-indentation)
(add-hook 'prog-mode-hook 'highlight-indentation-mode)
;(add-hook 'prog-mode-hook 'highlight-indentation-current-column-mode)
;(set-face-background 'highlight-indentation-face "#e3e3d3")
;(set-face-background 'highlight-indentation-current-column-face "#c3b3b3")

(require 'diff-hl)
(dolist (hook '(prog-mode-hook vc-dir-mode-hook conf-mode-hook))
  (add-hook hook 'turn-on-diff-hl-mode))
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)

(require 'guide-key)
(setq guide-key/guide-key-sequence
      '("C-x" "C-x 4" "C-x 5" "C-x 8" "C-x r" "C-x v" "C-x a" "C-x n" "C-x a"
        "C-x @" "C-x c" "C-c" "C-c h" "C-c @" "C-c C-x" "C-c c" "C-c &"
        "C-c !"))
(setq guide-key/popup-window-position 'bottom)
(guide-key-mode 1)

;; anzu
(require 'anzu)
(global-anzu-mode t)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
(custom-set-variables
 '(anzu-mode-lighter "")
 '(anzu-deactivate-region t)
 '(anzu-search-threshold 1000)
 '(anzu-replace-to-string-separator " => "))

 ;; ace-jump-mode
 ;; to be enable to M-x without meta
 (global-set-key (kbd "C-x C-m") 'execute-extended-command)
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; enable a more powerful jump back function from ace jump mode
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C-c c r") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c c c") 'mc/edit-lines)
(global-set-key (kbd "C-c c e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c c a") 'mc/edit-beginnings-of-lines)

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
                  (ignore-errors (previous-line 5))))

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
