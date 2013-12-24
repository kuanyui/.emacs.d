;;; umbrain.el --- Some brain training, ideas after Umeå brain training
;; 
;; Filename: umbrain.el
;; Description: 
;; Author: 
;; Maintainer: 
;; Created: Sun Aug  7 09:20:18 2011 (+0200)
;; Version: 
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(defun umbrain-letter ()
  (interactive)
  (let* ((chars "abcdefghijklmnopqrstuvwxyz")
         (numc (length chars))
         (selc nil)
         (len (+ 4 (random 4)))
         (nn 0)
         (shown nil)
         )
    ;; Select 6 chars
    (while (> 6 (length selc))
      (let ((cc (string-to-char (substring chars (random numc)))))
        (unless (memq cc selc)
          (setq selc (cons cc selc)))))
    (while (< (setq nn (1+ nn)) len)
      (let ((cc (nth (random 6) selc)))
        (setq shown (cons cc shown))
        (message "%c" cc)
        (sleep-for 2)
        (message "")
        (sleep-for 1)
        ))
    (read-string (format "Last four letters (%s): " (concat selc)))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; umbrain.el ends here
