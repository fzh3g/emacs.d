;; change font for better looking text
(cond
 (*linux*
  (set-face-attribute 'default nil :font "Monaco for Powerline 11"))
 (*is-a-mac*
  (set-face-attribute 'default nil :font "Monaco for Powerline 14"))
 (t
  (set-face-attribute 'default nil :font "Monaco for Powerline 12")))
;; Chinese Font
;; (dolist (charset '(han cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font)
;;                     charset
;;                     (font-spec :family "STKaiti")))

(provide 'init-fonts)
