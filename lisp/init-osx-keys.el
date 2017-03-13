;;; init-osx-keys.el --- Emacs configuration for OSX keys
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

;; Some configuration for OSX keys.

;;; Code:

(when *is-a-mac*
  (setq-default mac-command-modifier 'meta)
  (setq-default mac-option-modifier 'none)
  (setq-default default-input-method "MacOSX")
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key
       (read-kbd-macro (concat "<" multiple "wheel-" direction ">"))
       'ignore)))
  (global-set-key (kbd "M-`") 'ns-next-frame)
  (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
  (global-set-key (kbd "M-˙") 'ns-do-hide-others)
  (global-set-key (kbd "M-ˍ") 'ns-do-hide-others))

(provide 'init-osx-keys)
;;; init-osx-keys.el ends here
