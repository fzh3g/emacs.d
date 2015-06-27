(require 'ibuffer-vc)

(eval-after-load 'ibuffer
  '(progn
     ;; Use human readable Size column instead of original one
     (define-ibuffer-column size-h
       (:name "Size" :inline t)
       (cond
        ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
        ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
        (t (format "%8d" (buffer-size)))))))

(defun ibuffer-set-up-preferred-filters ()
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'filename/process)
    (ibuffer-do-sort-by-filename/process)))

(add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters)

(setq-default ibuffer-show-empty-filter-groups nil)

;; Modify the default ibuffer-formats (toggle with `)
(setq-default ibuffer-formats
              '((mark modified read-only vc-status-mini " "
                      (name 18 18 :left :elide)
                      " "
                      (size-h 9 -1 :right)
                      " "
                      (mode 16 16 :left :elide)
                      " "
                      filename-and-process)
                (mark modified read-only vc-status-mini " "
                      (name 18 18 :left :elide)
                      " "
                      (size-h 9 -1 :right)
                      " "
                      (mode 16 16 :left :elide)
                      " "
                      (vc-status 16 16 :left)
                      " "
                      filename-and-process)
                ))

(setq-default ibuffer-filter-group-name-face 'font-lock-doc-face)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
