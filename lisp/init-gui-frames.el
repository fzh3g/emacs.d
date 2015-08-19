;;----------------------------------------------------------------------
;; Supress GUI features
;;----------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

(setq initial-major-mode 'text-mode)
(setq-default initial-scratch-message
              (concat "Happy hacking " (or user-login-name "") "! *★,°*:.☆\(￣▽￣)/$:*.°★* \n"))

;; http://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
(setq ad-redefinition-action 'accept)

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

;; make the fringe thinner (default is 8 in pixels)
;(fringe-mode 6)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("(｡・`ω´･) • "
	(:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name))
		 "%b"))))

;; change font size
(when (display-graphic-p)
  (global-set-key (kbd "C-M-=") 'text-scale-increase)
  (global-set-key (kbd "C-M--") 'text-scale-decrease))

;; time management
(setq-default display-time-24hr-format t)
;(setq display-time-day-and-date t)
(display-time)

;; change font for better looking text
(set-face-attribute
   'default nil :font "Monofur for Powerline 12")
;(cond
; ((member "Monofur for Powerline" (font-family-list))
;  (set-face-attribute
;   'default nil :font "Monofur for Powerline 13"))
; ((member "MonacoB" (font-family-list))
;  (set-face-attribute
;   'default nil :font "MonacoB 10"))
; ((member "Menlo" (font-family-list))
;  (set-face-attribute
;   'default nil :font "Menlo 10"))
; ((member "Consolas" (font-family-list))
;  (set-face-attribute
;   'default nil :font "Consolas 11"))
; ((member "Dejavu Sans Mono" (font-family-list))
;  (set-face-attribute
;   'default nil :font "Dejavu Sans Mono 10")))

;; Chinese Font
;(dolist (charset '(kana han symbol cjk-misc bopomofo))
;    (set-fontset-font (frame-parameter nil 'font)
;                      charset
;                      (font-spec :family "Microsoft Yahei" :size 12)))


(provide 'init-gui-frames)

