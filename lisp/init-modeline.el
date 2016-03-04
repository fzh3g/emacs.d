;; spaceline
(use-package powerline
  :config
  (progn
    (setq powerline-default-separator 'wave)
    ))

;;----------------------------------------------------------------------
;; clean mode-line
;;----------------------------------------------------------------------
(defvar mode-line-cleaner-alist
  `((company-mode . "")
    (yas-minor-mode . "")
    (hs-minor-mode . "")
    (eldoc-mode . "")
    (abbrev-mode . "")
    (guide-key-mode . "")
    (undo-tree-mode . "")
    (highlight-symbol-mode . "")
    (helm-mode . "")
    (smartparens-mode . "")
    (hi-lock-mode . "")
    (which-key-mode . "")
    (page-break-lines-mode . "")
    (projectile-mode . "")
    (subword-mode . "")
    ;; Major modes
    (lisp-interaction-mode . "λ")
    (python-mode . "Py")
    (emacs-lisp-mode . "ELisp"))
  "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

(defun clean-mode-line ()
  "Clean minor/major mode in modeline."
  (interactive)
  (dolist (cleaner mode-line-cleaner-alist)
    (let* ((mode (car cleaner))
           (mode-str (cdr cleaner))
           (old-mode-str (cdr (assq mode minor-mode-alist))))
      (when old-mode-str
        (setcar old-mode-str mode-str))
      ;; major mode
      (when (eq mode major-mode)
        (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;; (defvar fx-projectile-mode-line
;;   '(:propertize
;;     (:eval (when (ignore-errors (projectile-project-root))
;;              (concat " [" (projectile-project-name) "]")))
;;     face font-lock-constant-face)
;;   "Mode line format for Projectile.")
;; (put 'fx-projectile-mode-line 'risky-local-variable t)

;; (defvar fx-vc-mode-line
;;   '(" " (:propertize
;;          ;; Strip the backend name from the VC status information
;;          (:eval (let ((backend (symbol-name (vc-backend (buffer-file-name)))))
;;                   (substring vc-mode (+ (length backend) 2))))
;;          face font-lock-variable-name-face))
;;   "Mode line format for VC Mode.")
;; (put 'fx-vc-mode-line 'risky-local-variable t)

;; ;; mode-line format
;; (setq-default mode-line-format
;;               (list
;;                ;; window-numbering
;;                ;'(:eval (window-numbering-get-number-string))
;;                " "
;;                ;; the buffer name; the file name as a tool tip
;;                '(:eval (propertize " %b "
;;                                    'face 'font-lock-function-name-face
;;                                    'help-echo (buffer-file-name)))
;;                ;; the current major mode for the buffer.
;;                '(:eval (propertize "%m"
;;                                    'face 'font-lock-type-face
;;                                    'help-echo buffer-file-coding-system))
;;                " "
;;                ;; insert vs overwrite mode, input-method in a tooltip
;;                '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
;;                                    'face font-lock-preprocessor-face
;;                                    'help-echo
;;                                    (concat "Buffer is in "
;;                                            (if overwrite-mode
;;                                                "overwrite"
;;                                              "insert") " mode")))
;;                ;; was this buffer modified since the last save?
;;                '(:eval (when (buffer-modified-p)
;;                          (concat
;;                           " "
;;                           (propertize "Mod"
;;                                       'face 'font-lock-warning-face
;;                                       'help-echo "Buffer has been modified"))))
;;                ;; is this buffer read-only?
;;                '(:eval (when buffer-read-only
;;                          (concat
;;                           " "  (propertize "RO"
;;                                            'face 'font-lock-type-face
;;                                            'help-echo "Buffer is read-only"))))
;;                " "
;;                ;; line and column
;;                "(" ;; '%02' to set to 2 chars at least; prevents flickering
;;                "%02l" "," "%02c"
;;                ")"
;;                " "
;;                ;; relative position
;;                (propertize "%p"
;;                            'face 'font-lock-keyword-face
;;                            )
;;                ;; projectile
;;                '(projectile-mode fx-projectile-mode-line)
;;                ;; vc information
;;                '(vc-mode fx-vc-mode-line)
;;                ;; flycheck status
;;                '(flycheck-mode flycheck-mode-line)
;;                ;; number of cursors
;;                '(multiple-cursors-mode mc/mode-line)
;;                " "
;;                ;; add the time, with the date and the emacs uptime in the tooltip
;;                '(:eval (propertize
;;                         (format-time-string "%H:%M")
;;                         'face 'font-lock-string-face
;;                         'help-echo
;;                         (concat
;;                          (format-time-string
;;                           "%Y-%02m-%02d %02H:%02M:%02S %3a; ")
;;                          (emacs-uptime "Uptime:%hh"))))
;;                " "
;;                ;; date
;;                ;'(:eval (propertize (format-time-string "%Y-%02m-%02d %3a")
;;                ;                    'face 'font-lock-keyword-face
;;                ;                    ))
;;                ;" "
;;                ;; user
;;                ;"("
;;                ;"@"
;;                ;(propertize user-login-name
;;                ;            'face 'font-lock-variable-name-face
;;                ;            )
;;                ;")"
;;                "--"
;;                ;; i don't want to see minor-modes; but if you want, uncomment this:
;;                ;;minor-mode-alist  ;; list of minor modes
;;                "%-" ;; fill with '-'
;;                ))

(provide 'init-modeline)
