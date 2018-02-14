;;; init-web.el --- Emacs configuration for web
;; -*- coding: utf-8 -*-
;;
;; Copyright (c) 2015-2018 Faxiang Zheng
;;
;; Author: Faxiang Zheng <fxzheng0906@outlook.com>
;; URL: https://github.com/zhengfaxiang/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Some configuration for web-mode.

;;; Code:

(use-package web-mode
  :defer t
  :init
  (progn
    (add-hook 'web-mode-hook 'flycheck-mode)
    )
  :config
  (progn
    (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
    (with-eval-after-load 'smartparens
      (setq web-mode-enable-auto-pairing nil)

      (sp-local-pair 'web-mode "<% " " %>")
      (sp-local-pair 'web-mode "{ " " }")
      (sp-local-pair 'web-mode "<%= "  "  %>")
      (sp-local-pair 'web-mode "<%# "  " %>")
      (sp-local-pair 'web-mode "<%$ "  " %>")
      (sp-local-pair 'web-mode "<%@ "  " %>")
      (sp-local-pair 'web-mode "<%: "  " %>")
      (sp-local-pair 'web-mode "{{ "  " }}")
      (sp-local-pair 'web-mode "{% "  " %}")
      (sp-local-pair 'web-mode "{%- "  " %}")
      (sp-local-pair 'web-mode "{# "  " #}")))
  :mode
  (("\\.phtml\\'"      . web-mode)
   ("\\.tpl\\.php\\'"  . web-mode)
   ("\\.html\\'"       . web-mode)
   ("\\.htm\\'"        . web-mode)
   ("\\.[gj]sp\\'"     . web-mode)
   ("\\.as[cp]x\\'"    . web-mode)
   ("\\.erb\\'"        . web-mode)
   ("\\.mustache\\'"   . web-mode)
   ("\\.handlebars\\'" . web-mode)
   ("\\.hbs\\'"        . web-mode)
   ("\\.eco\\'"        . web-mode)
   ("\\.djhtml\\'"     . web-mode)))

(use-package web-beautify
  :defer t
  :init
  (progn
    (eval-after-load 'js2-mode
      '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
    ;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
    (eval-after-load 'js
      '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))

    (eval-after-load 'json-mode
      '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))

    (eval-after-load 'sgml-mode
      '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))

    (eval-after-load 'web-mode
      '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))

    (eval-after-load 'css-mode
      '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))))

(provide 'init-web)
;;; init-web.el ends here
