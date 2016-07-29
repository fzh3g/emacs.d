;;; init-dired.el --- Emacs configuration for dired
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

;; Some configuration for dired and image-dired.

;;; Code:

(use-package dired
  :defer t)

(use-package dired+
  :defer t
  :init
  (progn
    (setq dired-recursive-deletes 'always)
    (setq diredp-hide-details-initially-flag t)
    (setq diredp-hide-details-propagate-flag t)
    (setq dired-dwim-target t)
    ;; use single buffer for all dired navigation
    (toggle-diredp-find-file-reuse-dir 1))
  :config
  (progn
    (when (fboundp 'global-dired-hide-details-mode)
      (global-dired-hide-details-mode -1))
    (define-key dired-mode-map (kbd "SPC") 'avy-goto-subword-1)
    (define-key dired-mode-map [mouse-2] 'dired-find-file)))

;; image-dired
(setq image-dired-dir (concat fx-cache-directory "image-dired/")
      image-dired-gallery-dir (concat image-dired-dir ".image-dired_gallery")
      image-dired-db-file (concat image-dired-dir ".image-dired_db")
      image-dired-temp-image-file (concat image-dired-dir ".image-dired_temp")
      image-dired-temp-rotate-image-file (concat image-dired-dir ".image-dired_rotate_temp"))

(provide 'init-dired)
;;; init-dired.el ends here
