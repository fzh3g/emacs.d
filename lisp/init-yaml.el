;;; init-yaml.el --- Emacs configuration for Yaml
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

;; Some configuration for yaml-mode.

;;; Code:

(use-package yaml-mode
  :mode "\\.yml$"
  :config
  (progn
    (define-key yaml-mode-map "\C-m" 'newline-and-indent)
    ))

(provide 'init-yaml)
;;; init-yaml.el ends here
