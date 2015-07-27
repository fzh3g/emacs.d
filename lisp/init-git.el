(require 'magit)
(require 'diff-hl)

(dolist
    (hook '(prog-mode-hook vc-dir-mode-hook conf-mode-hook markdown-mode-hook))
  (add-hook hook 'turn-on-diff-hl-mode))
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)

(global-set-key (kbd "M-<f12>") 'magit-status)

(setq-default magit-save-some-buffers nil
              magit-process-popup-time 10
              magit-completing-read-function 'magit-ido-completing-read)

(eval-after-load 'magit
  '(progn
     ;; Don't let magit-status mess up window configurations
     ;; http://whattheemacsd.com/setup-magit.el-01.html
     (defadvice magit-status (around magit-fullscreen activate)
       (window-configuration-to-register :magit-fullscreen)
       ad-do-it
       (delete-other-windows))

     (defun magit-quit-session ()
       "Restores the previous window configuration and kills the magit buffer"
       (interactive)
       (kill-buffer)
       (jump-to-register :magit-fullscreen))

     (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)))

(when *is-a-mac*
  (after-load 'magit
              (add-hook 'magit-mode-hook (lambda () (local-unset-key (kbd "M-h"))))))

(setq-default magit-auto-revert-mode t)

(provide 'init-git)
;;; init-git.el ends here
