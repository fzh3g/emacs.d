;;; init-css-mode.el --- Emacs configuration for css
;;
;; Copyright (c) 2015-2016 Faxiang Zheng
;;
;; Author: Faxiang Zheng <fxzheng0906@outlook.com>
;; URL: https://github.com/zhengfaxiang/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Some configuration for css-mode scss-mode less-css-mode.
;; and sass-mode.

;;; Code:

(use-package scss-mode
  :defer t
  :init
  (progn
    (add-hook 'scss-mode-hook 'flycheck-mode)
    (add-hook 'scss-mode-hook 'rainbow-delimiters-mode))
  :mode ("\\.scss\\'" . scss-mode))

(use-package less-css-mode
  :defer t
  :init
  (add-hook 'less-css-mode-hook 'rainbow-delimiters-mode)
  :mode ("\\.less\\'" . less-css-mode))

(use-package sass-mode
  :defer t
  :init
  (add-hook 'sass-mode-hook 'flycheck-mode)
  :mode ("\\.sass\\'" . sass-mode))

(provide 'init-css)
;;; init-css.el ends here
