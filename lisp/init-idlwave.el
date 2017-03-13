;;; init-idlwave.el --- Emacs configuration for IDLWave
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

;; Some configuration for IDLWave.

;;; Code:

(use-package idlwave
  :defer t
  :init
  (add-hook 'idlwave-mode-hook
            #'(lambda ()
                (flycheck-mode -1)
                (auto-fill-mode -1)
                ;; conflict with helm-ag and helm-swoop
                (define-key idlwave-mode-map (kbd "M-s") nil))))

(provide 'init-idlwave)
;;; init-idlwave.el ends here
