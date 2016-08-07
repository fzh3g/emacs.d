;;; init-yaml.el --- Emacs configuration for Yaml
;; -*- coding: utf-8 -*-
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
  :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
         ("Procfile\\'" . yaml-mode))
  :config
  (progn
    (add-hook 'yaml-mode-hook
              '(lambda ()
                 (define-key yaml-mode-map "\C-m" 'newline-and-indent)))))

(provide 'init-yaml)
;;; init-yaml.el ends here
