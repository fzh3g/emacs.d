;;; init-tramp.el --- Emacs configuration for tramp
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

;; Some configuration for tramp.

;;; Code:

;; http://www.quora.com/Whats-the-best-way-to-edit-remote-files-from-Emacs
(setq tramp-default-method "ssh")
(setq tramp-chunksize 8192)

;; Auto-save file
(setq auto-save-default t)
(setq auto-save-list-file-prefix (concat fx-auto-save-directory))
;; always save TRAMP URLs to cache directory
(let ((autosave-dir (concat fx-auto-save-directory "dist/")))
  (setq auto-save-file-name-transforms
        `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,autosave-dir  t)))
  (unless (file-exists-p autosave-dir)
    (make-directory autosave-dir)))
;; auto-save location
(let ((autosave-dir (concat fx-auto-save-directory "site/")))
  (add-to-list 'auto-save-file-name-transforms
               `(".*" ,autosave-dir t) 'append)
  (unless (file-exists-p autosave-dir)
    (make-directory autosave-dir t)))

;; https://github.com/syl20bnr/spacemacs/issues/1921
;; (setq tramp-ssh-controlmaster-options
;;       "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

(provide 'init-tramp)
;;; init-tramp.el ends here
