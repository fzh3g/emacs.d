;; change font for better looking text
(cond
 (*linux*
  (set-face-attribute 'default nil :font "Monaco 11"))
 (*is-a-mac*
  (set-face-attribute 'default nil :font "Monaco 14"))
 (t
  (set-face-attribute 'default nil :font "Monaco 12")))
;; Chinese Font
;; (dolist (charset '(han cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font)
;;                     charset
;;                     (font-spec :family "STKaiti")))
;; change font size
(when (display-graphic-p)
  (global-set-key (kbd "C-M-=") 'text-scale-increase)
  (global-set-key (kbd "C-M--") 'text-scale-decrease)
  (global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
  (global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease))

(provide 'init-fonts)