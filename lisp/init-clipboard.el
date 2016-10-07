;;; init-clipboard.el --- Emacs configuration for clipboard
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

;; Some configuration for copy and paste.

;;; Code:

;; Use the system clipboard
(setq select-enable-clipboard t
      select-enable-primary t)

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)


(provide 'init-clipboard)
;;; init-clipboard.el ends here
