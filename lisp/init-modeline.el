(setq-default mode-line-format
              (list
               ;; window-numbering
               ;'(:eval (window-numbering-get-number-string))
               " "
               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize " %b "
                                   'face 'font-lock-function-name-face
                                   'help-echo (buffer-file-name)))
               " "
               ;; the current major mode for the buffer.
               
               "["
               '(:eval (propertize "%m"
                                   'face 'font-lock-type-face
                                   'help-echo buffer-file-coding-system))
               " "
               ;; insert vs overwrite mode, input-method in a tooltip
               '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                                   'face font-lock-preprocessor-face
                                   'help-echo
                                   (concat "Buffer is in "
                                           (if overwrite-mode
                                               "overwrite"
                                             "insert") " mode")))
               ;; was this buffer modified since the last save?
               '(:eval (when (buffer-modified-p)
                         (concat
                          ","
                          (propertize "Mod"
                                      'face 'font-lock-warning-face
                                      'help-echo "Buffer has been modified"))))
               ;; is this buffer read-only?
               '(:eval (when buffer-read-only
                         (concat
                          ","  (propertize "RO"
                                           'face 'font-lock-type-face
                                           'help-echo "Buffer is read-only"))))
               "]"
               " "
               ;; line and column
               "(" ;; '%02' to set to 2 chars at least; prevents flickering
               "%02l" "," "%02c"
               ")"
               " "
               ;; relative position
               "["
               (propertize "%p"
                           'face 'font-lock-constant-face
                           )
               "]"
               " "
               ;; add the time, with the date and the emacs uptime in the tooltip
               '(:eval (propertize
                        (format-time-string "%H:%M")
                        'face 'font-lock-string-face
                        'help-echo
                        (concat
                         (format-time-string
                          "%Y-%02m-%02d %02H:%02M:%02S %Y-%02m-%02d %3a; ")
                         (emacs-uptime "Uptime:%hh"))))
               " "
               ;; date
               '(:eval (propertize (format-time-string "%Y-%02m-%02d %3a")
                                   'face 'font-lock-keyword-face
                                   ))
               " "
               ;; user
               "("
               "@"
               (propertize user-login-name
                           'face 'font-lock-variable-name-face
                           )
               ")"
               " "
               "--"
               ;; i don't want to see minor-modes; but if you want, uncomment this:
               ;;minor-mode-alist  ;; list of minor modes
               "%-" ;; fill with '-'
               ))

(provide 'init-modeline)
