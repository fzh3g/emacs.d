;; winner
(use-package winner
  :init
  (progn
    (winner-mode 1)
    (global-set-key (kbd "C-x 4 u") 'winner-undo)
    (global-set-key (kbd "C-x 4 r") 'winner-redo)))

;; When splitting window, show (other-buffer) in the new window
(defun fx/split-window-vertically ()
  (interactive)
  (split-window-vertically)
  (set-window-buffer (next-window) (other-buffer)))

(defun fx/split-window-horizontally ()
  (interactive)
  (split-window-horizontally)
  (set-window-buffer (next-window) (other-buffer)))

(global-set-key "\C-x2" #'fx/split-window-vertically)
(global-set-key "\C-x3" #'fx/split-window-horizontally)

;; Rearrange split windows
(defun fx/split-window-horizontally-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (fx/split-window-horizontally)))

(defun fx/split-window-vertically-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (fx/split-window-vertically)))

(global-set-key "\C-x|" #'fx/split-window-horizontally-instead)
(global-set-key "\C-x_" #'fx/split-window-vertically-instead)

(provide 'init-windows)
