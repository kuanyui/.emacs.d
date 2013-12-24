;;; xclip.el --- Emacs Interface to XClip

;; Copyright (C) 2007, 2012  Free Software Foundation, Inc.

;; Author: Leo Liu <sdl.web@gmail.com>
;; Keywords: convenience, tools
;; Created: 2007-12-30
;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This code provides an Emacs interface to the tool with the same
;; name on http://people.debian.org/~kims/xclip/.
;; Just add (xclip-mode 1) to your ~/.emacs.

;;; Code:
(defvar xclip-program "xclip"
  "Name of XClip program tool.")

(defvar xclip-select-enable-clipboard t
  "Non-nil means cutting and pasting uses the clipboard.
This is in addition to, but in preference to, the primary selection.")

(defvar xclip-last-selected-text-clipboard nil
  "The value of the CLIPBOARD X selection from xclip.")

(defvar xclip-last-selected-text-primary nil
  "The value of the PRIMARY X selection from xclip.")

(defun xclip-set-selection (type data)
  "TYPE is a symbol: primary, secondary and clipboard.

See `x-set-selection'."
  (when (and (executable-find xclip-program) (getenv "DISPLAY"))
    (let* ((process-connection-type nil)
           (proc (start-process "xclip" nil xclip-program
                                "-selection" (symbol-name type))))
      (process-send-string proc data)
      (process-send-eof proc))))

(defun xclip-select-text (text &optional push)
  "See `x-select-text'."
  (xclip-set-selection 'primary text)
  (setq xclip-last-selected-text-primary text)
  (when xclip-select-enable-clipboard
    (xclip-set-selection 'clipboard text)
    (setq xclip-last-selected-text-clipboard text)))

(defun xclip-selection-value ()
  "See `x-cut-buffer-or-selection-value'."
  (when (and (executable-find xclip-program) (getenv "DISPLAY"))
    (let (clip-text primary-text)
      (when xclip-select-enable-clipboard
        (setq clip-text (shell-command-to-string
                         (concat (shell-quote-argument xclip-program)
                                 " -o -selection clipboard")))
        (setq clip-text
              (cond ;; check clipboard selection
               ((or (not clip-text) (string= clip-text ""))
                (setq xclip-last-selected-text-primary nil))
               ((eq      clip-text xclip-last-selected-text-clipboard) nil)
               ((string= clip-text xclip-last-selected-text-clipboard)
                ;; Record the newer string,
                ;; so subsequent calls can use the `eq' test.
                (setq xclip-last-selected-text-clipboard clip-text)
                nil)
               (t (setq xclip-last-selected-text-clipboard clip-text)))))
      (setq primary-text (shell-command-to-string
                          (concat (shell-quote-argument xclip-program) " -o")))
      (setq primary-text
            (cond ;; check primary selection
             ((or (not primary-text) (string= primary-text ""))
              (setq xclip-last-selected-text-primary nil))
             ((eq      primary-text xclip-last-selected-text-primary) nil)
             ((string= primary-text xclip-last-selected-text-primary)
              ;; Record the newer string,
              ;; so subsequent calls can use the `eq' test.
              (setq xclip-last-selected-text-primary primary-text)
              nil)
             (t (setq xclip-last-selected-text-primary primary-text))))
      (or clip-text primary-text))))

(defun turn-on-xclip ()
  (setq interprogram-cut-function 'xclip-select-text)
  (setq interprogram-paste-function 'xclip-selection-value))

(defun turn-off-xclip ()
  (setq interprogram-cut-function nil)
  (setq interprogram-paste-function nil))

;;;###autoload
(define-minor-mode xclip-mode
  "Minor mode to use the `xclip' program to copy&paste."
  :global t
  (if xclip-mode
      (add-hook 'terminal-init-xterm-hook 'turn-on-xclip)
    (remove-hook 'terminal-init-xterm-hook 'turn-on-xclip)))

(provide 'xclip)
;;; xclip.el ends here
