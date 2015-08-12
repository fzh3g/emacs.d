(use-package recentf
  :defer t
  :init
  (progn
    (setq recentf-save-file (concat fx-cache-directory "recentf"))
    ;; lazy load recentf
    (add-hook 'find-file-hook (lambda () (unless recentf-mode
                                       (recentf-mode)
                                       (recentf-track-opened-file)))))
  :config
  (progn
    ;; @see http://stackoverflow.com/questions/2068697/emacs-is-slow-opening-recent-files
    (setq-default recentf-keep '(file-remote-p file-readable-p))
    (setq-default recentf-max-saved-items 100
                  recentf-exclude '("/tmp/"
                                    "/ssh:"
                                    "/sudo:"
                                    "/home/[a-z]\+/\\."))))

(provide 'init-recentf)
