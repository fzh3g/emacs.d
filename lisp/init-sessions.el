;; cache files
(setq bookmark-default-file (concat fx-cache-directory "bookmarks")
      bookmark-save-flag 1
      url-configuration-directory (concat fx-cache-directory "url")
      eshell-directory-name (concat fx-cache-directory "eshell" )
      tramp-persistency-file-name (concat fx-cache-directory "tramp"))

;; back up
(if (not (file-exists-p (expand-file-name "~/.backups")))
    (make-directory (expand-file-name "~/.backups")))
(setq make-backup-files t
      backup-directory-alist `(("." . "~/.backups"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-old-versions 3
      kept-new-versions 2)

(setq kill-ring-max 128)

(setq session-save-file (concat fx-cache-directory ".session"))
(setq session-save-print-spec '(t nil 40000))

(use-package desktop
  :init
  (progn
    (setq desktop-path (list fx-cache-directory))
    (setq desktop-save 'if-exists)
    (setq-default desktop-globals-to-save
                  (append '((extended-command-history . 128)
                            (file-name-history        . 128)
                            (ido-last-directory-list  . 128)
                            (kill-ring                . 128)
                            (ido-work-file-list       . 128)
                            (grep-history             . 128)
                            (compile-history          . 128)
                            (minibuffer-history       . 128)
                            (query-replace-history    . 128)
                            (read-expression-history  . 128)
                            (regexp-history           . 128)
                            (regexp-search-ring       . 128)
                            (search-ring              . 128)
                            (comint-input-ring        . 128)
                            (shell-command-history    . 128)
                            desktop-missing-file-warning
                            register-alist))) 
    (desktop-save-mode 1)))

(use-package savehist
  :defer t
  :init
  (progn
    (setq savehist-file (concat fx-cache-directory "savehist")
          history-length 2048
          savehist-autosave-interval 60
          savehist-additional-variables '(search-ring
                                          regexp-search-ring
                                          extended-command-history))
    (savehist-mode t)))

(provide 'init-sessions)
