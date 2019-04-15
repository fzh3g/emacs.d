;;; init-vimrc.el --- Emacs configuration for vimrc
;; -*- coding: utf-8 -*-
;;
;; Copyright (c) 2015-2019 Faxiang Zheng
;;
;; Author: Faxiang Zheng <fxzheng0906@outlook.com>
;; URL: https://github.com/zhengfaxiang/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Some configuration for editing vimrc

;;; Code:

(use-package vimrc-mode
  :mode "\\.vim[rc]?\\'"
  :mode "_vimrc\\'"
  :defer t
  :init
  (progn
    (add-hook 'vimrc-mode-hook
              #'(lambda ()
                  (rainbow-delimiters-mode-disable)))))

(provide 'init-vimrc)
;;; init-vimrc.el ends here
