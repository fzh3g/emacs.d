;; change font for better looking text
(use-package  chinese-fonts-setup
  :config
  (progn
    ;; change font size
    (global-set-key (kbd "C-M-=") 'cfs-increase-fontsize)
    (global-set-key (kbd "C-M--") 'cfs-decrease-fontsize)
    (global-set-key (kbd "<C-mouse-4>") 'cfs-increase-fontsize)
    (global-set-key (kbd "<C-mouse-5>") 'cfs-decrease-fontsize)))
;; (set-face-attribute 'default nil :font "Monaco:pixelsize=14")
;; (dolist (charset '(kana han cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font)
;;                     charset
;;                     (font-spec :family "WenQuanYi Micro Hei Mono" :size 16)))


(provide 'init-fonts)
