;;; init-recentf.el --- Emacs configuration for recent file
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

;; Some configuration for recentf.

;;; Code:

(use-package recentf
  :defer t
  :init
  (progn
    (setq recentf-save-file (concat fx-cache-directory "recentf")
          recentf-max-saved-items 1000)
    ;; lazy load recentf
    (add-hook 'find-file-hook (lambda () (unless recentf-mode
                                       (recentf-mode)
                                       (recentf-track-opened-file)))))
  :config
  (progn
    ;; @see http://stackoverflow.com/questions/2068697/emacs-is-slow-opening-recent-files
    (setq recentf-keep '(file-remote-p file-readable-p))
    (setq recentf-exclude '("/tmp/"
                            "/ssh:"
                            "/sudo:"
                            "COMMIT_EDITMSG\\'"))
    (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
    (add-to-list 'recentf-exclude (expand-file-name fx-cache-directory))))

(provide 'init-recentf)
;;; init-recentf.el ends here
