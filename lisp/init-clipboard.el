;;; init-editing-utils.el --- Emacs configuration for clipboard
;; -*- coding: utf-8 -*-
;;
;; Copyright (c) 2015-2020 Faxiang Zheng
;;
;; Author: Faxiang Zheng <fxzheng0906@outlook.com>
;; URL: https://github.com/zhengfaxiang/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Some configuration for better editing clipboard.

;;; Code:

;; Use the system clipboard
(setq select-enable-clipboard t
      select-enable-primary t)

(defun copy-to-x-clipboard ()
  "Copy to clipboard, especially in a terminal."
  (interactive)
  (if (region-active-p)
      (progn
        (cond
         ((and (display-graphic-p) select-enable-clipboard)
          (gui-set-selection 'CLIPBOARD
           (buffer-substring (region-beginning) (region-end))))
         (t (shell-command-on-region (region-beginning) (region-end)
             (cond
              (*is-a-mac* "pbcopy")
              (*linux* "xsel -ib")))))
        (message "Yanked region to clipboard!")
        (deactivate-mark))
    (message "No region active; can't yank to clipboard!")))

(defun paste-from-x-clipboard()
  "Paste from clipboard, especially in a terminal."
  (interactive)
  (cond
   ((and (display-graphic-p) select-enable-clipboard)
    (insert (gui-get-selection 'CLIPBOARD)))
   (t (shell-command
       (cond
        (*is-a-mac* "pbpaste")
        (t "xsel -ob"))
       1))))

(global-set-key (kbd "C-c a") 'copy-to-x-clipboard)
(global-set-key (kbd "C-c y") 'paste-from-x-clipboard)

(add-hook 'minibuffer-setup-hook
          #'(lambda ()
              (local-set-key (kbd "M-y") 'paste-from-x-clipboard)))

(provide 'init-clipboard)
;;; init-clipboard.el ends here
