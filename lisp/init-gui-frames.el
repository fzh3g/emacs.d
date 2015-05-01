;;----------------------------------------------------------------------
;; Supress GUI features
;;----------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

(setq-default initial-scratch-message
              (concat ";; Happy hacking " (or user-login-name "") "!\n\n"))

;; Show a marker in the left fringe for lines not in the buffer
(setq indicate-empty-lines t)

;; Window size and features
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(menu-bar-mode -1)

;; adjust opacity
(defun adjust-opacity (frame incr)
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(global-set-key (kbd "<f11>") 'toggle-frame-fullscreen)
(global-set-key (kbd "M-<f11>") 'toggle-frame-maximized)
(global-set-key (kbd "M-C-8") (lambda ()
				(interactive)
				(adjust-opacity nil -5)))
(global-set-key (kbd "M-C-9") (lambda ()
				(interactive)
				(adjust-opacity nil 5)))
(global-set-key (kbd "M-C-0") (lambda ()
				(interactive)
				(modify-frame-parameters nil `((alpha . 100)))))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (unless window-system
                (set-frame-parameter nil 'menu-bar-lines 0)))))

;; Toggle line highlighting in all buffers
(global-hl-line-mode t)

;; file column indicator
(require 'fill-column-indicator)
(setq default-fill-column 80)
(setq fill-column 80)
(setq fci-column 80)
(add-hook 'prog-mode-hook 'fci-mode)

;; make the fringe thinner (default is 8 in pixels)
(fringe-mode 6)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '(" ϕωϕ • "
	(:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name))
		 "%b"))))

;; change font for better looking text
(if (member "MonacoB" (font-family-list))
    (set-face-attribute
     'default nil :font "MonacoB 10"))

;; Chinese Font
(dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec :family "Microsoft Yahei" :size 12)))

;; change font size
(when (display-graphic-p)
  (global-set-key (kbd "C-M-=") 'text-scale-increase)
  (global-set-key (kbd "C-M--") 'text-scale-decrease))

;; time management
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)

(provide 'init-gui-frames)

