;;; init-dired.el --- Emacs configuration for dired
;; -*- coding: utf-8 -*-
;;
;; Copyright (c) 2015-2017 Faxiang Zheng
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
  :defer t
  :bind (("C-x C-j" . dired-jump)
         ("C-x d" . dired))
  :config
  (progn
    (setq dired-dwim-target t
          dired-recursive-copies 'top
          dired-recursive-deletes 'top)
    (use-package dired+
      :config
      (setq diredp-hide-details-initially-flag t)
      ;; use single buffer for all dired navigation
      (add-hook 'dired-mode-hook
                (lambda () (toggle-diredp-find-file-reuse-dir 1)))
      (define-key dired-mode-map (kbd "C-c p")
        'diredp-up-directory-reuse-dir-buffer))
    (use-package dired-narrow
      :bind (:map dired-mode-map
                  ("/" . dired-narrow)))
    (use-package dired-subtree
      :bind (:map dired-mode-map
                  ("<tab>" . dired-subtree-toggle)
                  ("C-c C-u" . dired-subtree-up)
                  ("C-c C-d" . dired-subtree-down)
                  ("C-c C-p" . dired-subtree-previous-sibling)
                  ("C-c C-n" . dired-subtree-next-sibling)))))

;; image-dired
(use-package image-dired
  :defer t
  :config
  (progn
    (setq image-dired-dir (concat fx-cache-directory "image-dired/")
          image-dired-gallery-dir (concat image-dired-dir
                                          ".image-dired_gallery")
          image-dired-db-file (concat image-dired-dir ".image-dired_db")
          image-dired-temp-image-file (concat image-dired-dir
                                              ".image-dired_temp")
          image-dired-temp-rotate-image-file (concat
                                              image-dired-dir
                                              ".image-dired_rotate_temp"))))

(provide 'init-dired)
;;; init-dired.el ends here
