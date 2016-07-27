;;; init-www.el --- Emacs configuration for World Wide Web
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

;; Some configuration for google searching.

;;; Code:

;; default browser
(setq browse-url-browser-function 'browse-url-default-browser)

;; google this
(use-package google-this
  :diminish google-this-mode
  :defer t
  :init
  (progn
    (google-this-mode 1)))

(provide 'init-www)
;;; init-www.el ends here
