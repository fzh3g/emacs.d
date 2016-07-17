;;; init-isearch.el --- Emacs configuration for isearch
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

;; Some configuration for isearch.

;;; Code:

(use-package isearch
  :defer t
  :init
  (progn
    ;; Activate occur easily inside isearch
    (define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

    ;; Search back/forth for the symbol at point
    ;; See http://www.emacswiki.org/emacs/SearchAtPoint
    (defun isearch-yank-symbol ()
      "*Put symbol at current point into search string."
      (interactive)
      (let ((sym (symbol-at-point)))
        (if sym
            (progn
              (setq isearch-regexp t
                    isearch-string (concat "\\_<" (regexp-quote (symbol-name sym)) "\\_>")
                    isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
                    isearch-yank-flag t))
          (ding)))
      (isearch-search-and-update))

    (define-key isearch-mode-map (kbd "C-M-w") 'isearch-yank-symbol)

    ;; http://www.emacswiki.org/emacs/ZapToISearch
    (defun zap-to-isearch (rbeg rend)
      "Kill the region between the mark and the closest portion of
the isearch match string. The behaviour is meant to be analogous
to zap-to-char; let's call it zap-to-isearch. The deleted region
does not include the isearch word. This is meant to be bound only
in isearch mode.  The point of this function is that oftentimes
you want to delete some portion of text, one end of which happens
to be an active isearch word. The observation to make is that if
you use isearch a lot to move the cursor around (as you should,
it is much more efficient than using the arrows), it happens a
lot that you could just delete the active region between the mark
and the point, not include the isearch word."
      (interactive "r")
      (when (not mark-active)
        (error "Mark is not active"))
      (let* ((isearch-bounds (list isearch-other-end (point)))
             (ismin (apply 'min isearch-bounds))
             (ismax (apply 'max isearch-bounds))
             )
        (if (< (mark) ismin)
            (kill-region (mark) ismin)
          (if (> (mark) ismax)
              (kill-region ismax (mark))
            (error "Internal error in isearch kill function.")))
        (isearch-exit)
        ))

    (define-key isearch-mode-map (kbd "M-z") 'zap-to-isearch)

    ;; http://www.emacswiki.org/emacs/ZapToISearch
    (defun isearch-exit-other-end (rbeg rend)
      "Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill."
      (interactive "r")
      (isearch-exit)
      (goto-char isearch-other-end))

    (define-key isearch-mode-map (kbd "C-<return>") 'isearch-exit-other-end)))

(provide 'init-isearch)
;;; init-isearch.el ends here
