;;; smart-window.el --- vim-like window controlling plugin

;; Copyright (C) 2013 by Felix Chern

;; Author: Felix Chern <idryman@gmail.com>
;; URL: https://github.com/dryman/smart-window.el
;; Version: 0.6
;; Created: Feb 12 2013
;; Keywords: window
;; Compatibility: Emacs 24 and above

;; This file is NOT part of GNU Emacs

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package improves Emacs window system with window moving
;; commands (not moving focus between windows) and window
;; splitting commands that takes addtional options to edit a file
;; or a buffer.

;; C-x w runs the command smart-window-move, which is an interactive
;;   function that move the current window to specified directions
;;   (left/right/above/Below).

;; C-x W runs the command smart-window-buffer-split, which is an
;;   interactive function that asks you to choose a buffer and creates
;;   a new splitted window with coresponding buffer.

;; C-x M-w runs the command smart-window-file-split, which is an
;;   interactive function that asks you to choose a file and creates
;;   a new splitted window with coresponding file.

;; C-x R runs the command smart-window-rotate, which is an interactive
;;   function that rotates the windows downwards and rightwards

;; M-x sw-above allows you to split a window above the current window
;;   with the buffer you chose.

;; M-x sw-below allows you to split a window below the current window
;;   with the buffer you chose.

;; M-x sw-left allows you to split a window left to the current window
;;   with the buffer you chose.

;; M-x sw-right allows you to split a window right to the current window
;;   with the buffer you chose.

;; C-x 2 is bounded to sw-below.
;; C-x 3 is bounded to sw-right.
;; To switch back to the default key bindings, use the following settings in .emacs
;;   (setq smart-window-remap-keys 0)

;;; Code:
(defvar smart-window-remap-keys t)

(global-set-key (kbd "C-x w") 'smart-window-move)
(global-set-key (kbd "C-x W") 'smart-window-buffer-split)
(global-set-key (kbd "C-x M-w") 'smart-window-file-split)
(global-set-key (kbd "C-x R") 'smart-window-rotate)
(define-key (current-global-map) (kbd "C-x 2") (if smart-window-remap-keys 'sw-below 'split-window-below))
(define-key (current-global-map) (kbd "C-x 3") (if smart-window-remap-keys 'sw-right 'split-window-right))


;;;###autoload
(defun smart-window-move (edge)
  "Move the current window to the edge of the frame. The edge
options are 'left/right/above/below', where 'below' is the default.

For example, if you chose 'above', then the current window
would be at the very top, using the full width of the screen."
  (interactive 
   (list (completing--direction "Move window: ")))
      (let ((window (selected-window)))
    (select-window (split-window (frame-root-window) nil edge))
    (delete-window window)))

(defun smart-window-rotate ()
  "Rotate windows downwards and rightwards"
  (interactive)
  (let* ((window (selected-window))
         (w-list (window-list))
         (b-list (mapcar 'window-buffer w-list)))
    (mapcar* 'set-window-buffer
             w-list
             (append (last b-list) (butlast b-list)))
    (select-window (cadr w-list))))

;;;###autoload
(defun smart-window-buffer-split (buffer-name)
  "Split the current window, where new window content is not current
buffer but the buffer you picked from the minibuffer prompt."
  (interactive "BSelect buffer: ")
  (smart-window--split
   buffer-name
   (completing--direction "Direction: ")))

;;;###autoload
(defun smart-window-file-split (file-name)
  "Split the current window, where new window content is not current
buffer but the file you picked form the minibuffer prompt."
  (interactive "FSelect file: ")
  (smart-window--split
   (find-file-noselect file-name)
   (completing--direction "Direction: ")))

;;;###autoload
(defun sw-below (buffer-name)
  "Split current window with new window at below. The new window
content is the buffer you picked from the minibuffer prompt."
  (interactive "BSelect buffer: ")
  (smart-window--split buffer-name 'below))

;;;###autoload
(defun sw-above (buffer-name)
  "Split current window with new window at above. The new window
content is the buffer you picked from the minibuffer prompt."
  (interactive "BSelect buffer: ")
  (smart-window--split buffer-name 'above))

;;;###autoload
(defun sw-left (buffer-name)
  "Split current window with new window left to the current one.
The new window content is the buffer you picked from the minibuffer prompt."
  (interactive "BSelect buffer: ")
  (smart-window--split buffer-name 'left))

;;;###autoload
(defun sw-right (buffer-name)
  "Split current window with new window right next to current one.
The new window content is the buffer you picked from the minibuffer prompt."
  (interactive "BSelect buffer: ")
  (smart-window--split buffer-name 'right))


;; internal functions

(defun smart-window--split (buffer-name dir)
  "Internal function that splits the window with given direction and buffer name"
  (set-window-buffer (select-window (split-window nil nil dir)) buffer-name))

(defun completing--direction (prompt)
  "Internal function that handles minibuffer input.
It returns symbols 'left 'right 'above or 'below."
  (intern (completing-read
           (concat prompt " (left/right/above/Below) ")
           (split-string "left right above below")
           nil t nil nil "below")))

(provide 'smart-window)
;;; smart-window.el ends here
