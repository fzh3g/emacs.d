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
(setq tramp-auto-save-directory "~/.backups/tramp/")
(setq tramp-chunksize 8192)

;; https://github.com/syl20bnr/spacemacs/issues/1921
(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

;; sudo save
(defun sudo-save ()
  (interactive)
  (write-file (concat "/sudo:root@localhost:" buffer-file-name)))
(global-set-key (kbd "C-x M-s") #'sudo-save)

(provide 'init-tramp)
;;; init-tramp.el ends here
