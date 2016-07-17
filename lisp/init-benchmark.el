;;; init-benchmark.el --- Emacs configuration for benchmark
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

;; Some configuration for Emacs init time message.

;;; Code:

;; init time
(defun fx/time-subtract-millis (time-after time-before)
  "Time elapse between TIME-AFTER and TIME-BEFORE in millisecond."
  (* 1000.0 (float-time (time-subtract time-after time-before))))

(add-hook 'after-init-hook
          (lambda ()
            (message "init completed in %.2fms"
                     (fx/time-subtract-millis
                      after-init-time before-init-time))))

(provide 'init-benchmark)
;;; init-benchmark.el ends here
