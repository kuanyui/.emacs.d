;;; indent-hints.el --- Get some hints about whether your buffer is
;;; space- or tab-loving

;; Copyright (C) 2011, Mitchel Humpherys

;; Author: Mitchel Humpherys <mitch.special@gmail.com>
;; Keywords: convenience
;; Version: 0.1

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
;;
;; As the Eternal Holy War of tabs-versus-spaces rages on, even within
;; individual projects, an emacs minor mode arises from the depths of
;; github with the goal of easing the burden placed on the programmer
;; of trying to maintain consistency in text files.
;;
;; If you jump into a file that uses tabs for indentation, you shall
;; continue using tabs for indentation. If you jump into a file that
;; uses spaces for indentation, you shall continue using spaces for
;; indentation. That's the idea.
;;
;;; Installation:
;;
;; o For the impatient, here's a quick setup example (after putting
;;   indent-hints.el in your load path):
;;
;;     (require 'indent-hints)
;;     (indent-hints-global-mode)
;;
;;   You should probably at least customize the indent-hints-mode
;;   group to use your preferred space/tabs setup, like so:
;;
;;     M-x customize-group [RET] indent-hints [RET]
;;
;; o You can set up some "whitespace profiles" that get selected
;;   automatically when a buffer is detected to be tab-loving or
;;   space-loving. To enable this functionality, you should customize
;;   the `indent-hints-mode` group and enable
;;   indent-hints-profile-switching-enabled, or add to your .emacs:
;;
;;     (setq indent-hints-profile-switching-enabled t)
;;
;; o You can also add your own custom functions to the hooks
;;   `indent-hints-mode-tab-loving-hook` and
;;   `indent-hints-mode-space-loving-hook` which run after a buffer is
;;   detected to be tab-loving or space-loving, respectively.
;;
;;; Use:
;;
;; Just check out your mode-line to see whether the buffer you're
;; visiting is space-loving or tab-loving. It also shows the ratio of
;; space-to-tab (or tab-to-space, whichever your buffer loves)
;; loving-ness that your current buffer exudes. Here's a "screenshot":
;;
;;     test.el Top -- (Emacs-Lisp \t:0.53 yas pair)--etc. etc.--
;;
;; The file being visited in the "screenshot" has more tabs than
;; spaces (53% of the lines that start with some sort of indentation
;; start with tabs, to be exact).
;;
;;; Code

(defgroup indent-hints nil
  "Indent Hints Group"
  :group 'convenience)

;; (defcustom indent-hints-tab-loving-space-profile nil
;;   "The profile to activate when a space-loving buffer is detected"
;;   :type '(list :tag "How to set up spacees"
;;                (integer :tag "c-basic-offset" :value 4)
;;                (string :tag "c-default-style" :value "bsd"))
;;   :group 'indent-hints)

(defcustom indent-hints-profile-switching-enabled nil
  "Non-nil means switch between spacing profiles depending on the
  tab- or space-lovingness of buffers"
  :type 'boolean
  :group 'indent-hints)

(defcustom indent-hints-tab-width 4
  "When non-nil, the tab-width to use when the tab-loving profile
is enabled."
  :type 'integer
  :group 'indent-hints)

(defcustom indent-hints-c-basic-offset 4
  "When non-nil, the c-basic-offset to use when the space-loving
profile is enabled."
  :type 'integer
  :group 'indent-hints)

(defcustom indent-hints-c-default-style "bsd"
  "When non-nil, the c-default-style to use when the space-loving
  profile is enabled."
  :type 'string
  :group 'indent-hints)

(defvar indent-hints-mode-tab-loving-hook nil
  "Function(s) to call after detecting a tab-loving buffer")

(defvar indent-hints-mode-space-loving-hook nil
  "Function(s) to call after detecting a space-loving buffer")

(defvar indent-hints-mode-quiet-mode t
  "Set to nil to allow indent hints mode to print stuff about what it's doing")

(define-minor-mode indent-hints-mode
  "Give user hints about whether this buffer is space- or tab-loving."
  nil "" nil
  (if indent-hints-mode
      (let* ((vals (ih/count-line-beginnings))
             (begin-with-tab (nth 0 vals))
             (begin-with-space (nth 1 vals))
             (begin-with-something-else (nth 2 vals)))
        (ih/message "%s: %d lines, %d start with tabs, %d start with spaces"
                 (buffer-name)
                 (+ begin-with-something-else begin-with-tab begin-with-space)
                 begin-with-tab
                 begin-with-space)
        (cond
         ;; no tabs or spaces
         ((and (= 0 begin-with-tab) (= 0 begin-with-space))
          (setq tab-loving nil)
          (setq space-loving nil)
          (setq neither-loving t))
         ;; space-loving
         ((> begin-with-space begin-with-tab)
          (let ((space-ratio
                 (/ (float begin-with-space) (+ begin-with-space begin-with-tab))))
            (setq space-loving t)
            (setq tab-loving nil)
            (ih/update-space-loving-ratio space-ratio)
            (if indent-hints-profile-switching-enabled
                (ih/activate-space-loving-profile))
            (run-hooks 'indent-hints-mode-space-loving-hook)))
         ;; tab-loving
         (t
          (let ((tab-ratio
                 (/ (float begin-with-tab) (+ begin-with-space begin-with-tab))))
            (setq tab-loving t)
            (setq space-loving nil)
            (ih/update-tab-loving-ratio tab-ratio)
            (if indent-hints-profile-switching-enabled
                (ih/activate-tab-loving-profile))
            (run-hooks 'indent-hints-mode-tab-loving-hook))))) ; eo let,t,cond,let*
    ;; else, the mode was disabled:
    (progn
      (setq space-loving nil tab-loving nil)
      (ih/message "indent-hints-mode disabled!"))))


;;; Profile switching functions
;;

(defun ih/activate-space-loving-profile ()
  "Activate the space-loving profile"
  (interactive)
  (ih/message "Activating indent-hints space profile")
  (setq indent-tabs-mode nil)
  (if indent-hints-c-basic-offset
	  (setq c-basic-offset indent-hints-c-basic-offset))
  (if indent-hints-c-default-style
	  (setq c-default-style indent-hints-c-default-style)))

(defun ih/activate-tab-loving-profile ()
  "Activate the tab-loving profile"
  (interactive)
  (ih/message "Activating indent-hints tab profile")
  (setq indent-tabs-mode t)
  (if tab-width indent-hints-tab-width
    (setq tab-width indent-hints-tab-width)))


;;; Helper functions
;;

(defun ih/message (format-string &rest args)
  (interactive)
  (unless indent-hints-mode-quiet-mode
    (apply 'message format-string args)))

(defun ih/count-line-beginnings ()
  "The real meat. Examine the first character of each line in the
buffer. This can be used to determine if a buffer is space-loving
or tab-loving. Returns a list of the
form: (num-beginning-with-tab num-beginning-with-space
num-beginning-with-something-else)"
  (interactive)
  (save-excursion
    (let ((begin-with-tab 0)
          (begin-with-space 0)
          (begin-with-something-else 0)
          (current-line-number 1))
      (goto-char 1)
      (while (char-after (point))
        (cond ((= 32 (char-after (point)))
               (setq begin-with-space (1+ begin-with-space)))
              ((= 9 (char-after (point)))
               (setq begin-with-tab (1+ begin-with-tab)))
              (t (setq begin-with-something-else (1+ begin-with-something-else))))
        (setq current-line-number (1+ current-line-number))
        (forward-line 1)
        ) ;; eo while
      (list begin-with-tab begin-with-space begin-with-something-else)
      ) ;; eo let
    ) ;; eo save-excursion
  ) ;; eo defun

;; global variable to keep track of whether or not we've done global
;; activation of indent-hints-mode:
(setq indent-hints-did-global-activation nil)

(defcustom indent-hints-space-loving-modeline-indicator " \" \""
  "Modeline indicator to use when the file is space-loving")

(defcustom indent-hints-tab-loving-modeline-indicator " \\t"
  "Modeline indicator to use when the file is tab-loving")

(defcustom indent-hints-neither-loving-modeline-indicator ""
  "Modeline indicator to use when the file is neither-loving")

(defun indent-hints-global-activate ()
  "Sets up the minor-mode-alist and buffer-local variable for indentation hints"
  (ih/message "doing global-activate globact: %S" indent-hints-did-global-activation)
  (setq minor-mode-alist (cons '(space-loving indent-hints-space-loving-modeline-indicator)
                               (cons '(tab-loving indent-hints-tab-loving-modeline-indicator)
                                     (cons '(neither-loving indent-hints-neither-loving-modeline-indicator)
                                           minor-mode-alist))))
  (setq space-loving nil tab-loving nil neither-loving nil)
  (make-variable-buffer-local 'space-loving)
  (make-variable-buffer-local 'tab-loving)
  (make-variable-buffer-local 'neither-loving)
  (setq indent-hints-did-global-activation t))

(setq ih/love-sep ":")

(defun ih/update-space-loving-ratio (ratio)
  "Update the of space-loving-ness shown in the mode line"
  (interactive)
  (let ((newval (concat indent-hints-space-loving-modeline-indicator ih/love-sep (format "%.2f" ratio))))
       (setq minor-mode-alist
             (cons (list 'space-loving newval)
                   (assq-delete-all 'space-loving minor-mode-alist)))))

(defun ih/update-tab-loving-ratio (ratio)
  "Update the of tab-loving-ness shown in the mode line"
  (interactive)
  (let ((newval (concat indent-hints-tab-loving-modeline-indicator ih/love-sep (format "%.2f" ratio))))
       (setq minor-mode-alist
             (cons (list 'tab-loving newval)
                   (assq-delete-all 'tab-loving minor-mode-alist)))))

(defun indent-hints-mode-on ()
  "Turns on indent-hints-mode, if appropriate.
This function is intended to be used with define-globalized-minor-mode"
  (unless indent-hints-did-global-activation (indent-hints-global-activate))
  (unless (or
           indent-hints-mode
           (minibufferp)
           (ih/is-temp-buffer (buffer-name))
           (not (buffer-live-p (current-buffer))))
    (indent-hints-mode 1)))

(defun ih/is-temp-buffer (the-buffer-name)
  "Returns true if given buffer name is a temp buffer (starts with \" *\")"
  (string= (substring (car (split-string the-buffer-name)) 0 1) "*"))

(define-globalized-minor-mode indent-hints-global-mode indent-hints-mode indent-hints-mode-on
  :group 'indent-hints
  :require 'indent-hints)

(provide 'indent-hints)
;;; indent-hints.el ends here
;;
