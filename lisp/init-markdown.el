;;; init-markdown.el --- Emacs configuration for Markdown
;; -*- coding: utf-8 -*-
;;
;; Copyright (c) 2015-2017 Faxiang Zheng
;;
;; Author: Faxiang Zheng <fxzheng0906@outlook.com>
;; URL: https://github.com/zhengfaxiang/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Some configuration for markdown-mode.

;;; Code:

(use-package markdown-mode
  :mode "\\.\\(m[k]d\\|markdown\\)\\'"
  :defer t
  :config
  (progn
    (defun markdown-imenu-index ()
      (let* ((patterns '((nil "^#\\([# ]*[^#\n\r]+\\)" 1))))
        (save-excursion
          (imenu--generic-function patterns))))

    (add-hook 'markdown-mode-hook
              #'(lambda ()
                  (setq imenu-create-index-function 'markdown-imenu-create-index)))

    (when (fboundp 'sp-local-pair)
      (sp-local-pair 'markdown-mode "`" nil :actions '(:rem autoskip))
      (sp-local-pair 'markdown-mode "'" nil :actions nil))
    ))

(use-package mmm-mode
  :diminish mmm-mode
  :commands mmm-mode
  :init
  (add-hook 'markdown-mode-hook
            #'(lambda ()
                (mmm-mode 1)))
  :config
  (progn
    (mmm-add-classes '((markdown-ini
                        :submode conf-unix-mode
                        :face mmm-declaration-submode-face
                        :front "^```ini[\n\r]+"
                        :back "^```$")))
    (mmm-add-classes '((markdown-python
                        :submode python-mode
                        :face mmm-declaration-submode-face
                        :front "^```python[\n\r]+"
                        :back "^```$")))
    (mmm-add-classes '((markdown-html
                        :submode web-mode
                        :face mmm-declaration-submode-face
                        :front "^```html[\n\r]+"
                        :back "^```$")))
    (mmm-add-classes '((markdown-java
                        :submode java-mode
                        :face mmm-declaration-submode-face
                        :front "^```java[\n\r]+"
                        :back "^```$")))
    (mmm-add-classes '((markdown-ruby
                        :submode ruby-mode
                        :face mmm-declaration-submode-face
                        :front "^```ruby[\n\r]+"
                        :back "^```$")))
    (mmm-add-classes '((markdown-c
                        :submode c-mode
                        :face mmm-declaration-submode-face
                        :front "^```c[\n\r]+"
                        :back "^```$")))
    (mmm-add-classes '((markdown-c++
                        :submode c++-mode
                        :face mmm-declaration-submode-face
                        :front "^```c\+\+[\n\r]+"
                        :back "^```$")))
    (mmm-add-classes '((markdown-elisp
                        :submode emacs-lisp-mode
                        :face mmm-declaration-submode-face
                        :front "^```elisp[\n\r]+"
                        :back "^```$")))
    (mmm-add-classes '((markdown-javascript
                        :submode javascript-mode
                        :face mmm-declaration-submode-face
                        :front "^```javascript[\n\r]+"
                        :back "^```$")))
    (mmm-add-classes '((markdown-ess
                        :submode R-mode
                        :face mmm-declaration-submode-face
                        :front "^```{?r.*}?[\n\r]+"
                        :back "^```$")))
    (mmm-add-classes '((markdown-f90
                        :submode f90-mode
                        :face mmm-declaration-submode-face
                        :front "^```f90[\n\r]+"
                        :back "^```$")))
    (mmm-add-classes '((markdown-idl
                        :submode idlwave-mode
                        :face mmm-declaration-submode-face
                        :front "^```idl[\n\r]+"
                        :back "^```$")))
    (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-ini)
    (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-python)
    (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-java)
    (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-ruby)
    (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-c)
    (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-c++)
    (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-elisp)
    (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-html)
    (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-javascript)
    (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-ess)
    (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-f90)
    (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-idl)))

(provide 'init-markdown)
;;; init-markdown.el ends here
