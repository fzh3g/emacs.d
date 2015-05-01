;; Use C-f during file selection to regular find-file
(require 'ido-ubiquitous)
(require 'idomenu)
(require 'flx-ido)
(require 'ido-hacks nil t)

(ido-mode t)
(ido-ubiquitous-mode t)
;(ido-everywhere t)
(if (commandp 'ido-vertical-mode)
    (progn
      (ido-vertical-mode 1)
      (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)))
;; http://sachachua.com/blog/2014/03/emacs-basics-call-commands-name-m-x-tips-better-completion-using-ido-helm/

(if (commandp 'flx-ido-mode)
    (flx-ido-mode 1))
(setq ido-enabble-flex-matching t)
(setq ido-use-faces nil)
(setq ido-use-filename-at-point 'guess)
(setq ido-auto-merge-work-directories-length 0)
(setq ido-use-virtual-buffers t)

(setq ido-default-buffer-method 'selected-window)


(provide 'init-ido)
