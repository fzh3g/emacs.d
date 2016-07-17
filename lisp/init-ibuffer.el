;;; init-ibuffer.el --- Emacs configuration for ibuffer
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

;; Some configuration for ibuffer

;;; Code:

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer))
  :init
  (progn
    (setq ibuffer-show-empty-filter-groups nil)
    (setq ibuffer-filter-group-name-face 'font-lock-doc-face)
    )
  :config
  (progn
    (use-package ibuffer-vc
      :init
      (progn
        (setq ibuffer-formats
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
                      filename-and-process)))
        (defun ibuffer-set-up-preferred-filters ()
          (ibuffer-vc-set-filter-groups-by-vc-root)
          (unless (eq ibuffer-sorting-mode 'filename/process)
            (ibuffer-do-sort-by-filename/process)))
        (add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters)))
    (define-ibuffer-column size-h
      (:name "Size" :inline t)
      (cond
       ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
       ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
       (t (format "%8d" (buffer-size)))))))

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
