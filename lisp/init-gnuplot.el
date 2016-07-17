;;; init-gnuplot.el --- Emacs configuration for Gnuplot
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

;; Some configuration for gnuplot

;;; Code:

(use-package gnuplot
  :commands (gnuplot-mode
             gnuplot-make-buffer)
  :mode "\\.gp$")

(provide 'init-gnuplot)
;;; init-gnuplot.el ends here
