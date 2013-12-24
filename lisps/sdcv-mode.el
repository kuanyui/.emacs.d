;;; sdcv-mode.el --- major mode to do dictionary query through sdcv

;; Copyright 2006~2008 pluskid
;;
;; Author: pluskid@gmail.com
;; Version: $Id: sdcv-mode.el,v 0.1 2008/06/11 21:48:51 kid Exp $
;; Keywords: sdcv dictionary
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This is a major mode to view output of dictionary search of sdcv.

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;   (require 'sdcv-mode)
;;   (global-set-key (kbd "C-c d") 'sdcv-search)

;;; Code:

(require 'outline)
(provide 'sdcv-mode)
(eval-when-compile
  (require 'cl))

;;; ==================================================================
;;; Frontend, search word and display sdcv buffer
(defun sdcv-search (select-dictionary-list)
  "Prompt for a word to search through sdcv.
When provided with a prefix argument, select new
`sdcv-dictionary-list' before search.

Word may contain some special characters:
    *       match zero or more characters
    ?       match zero or one character
    /       used at the beginning, for fuzzy search
    |       used at the beginning, for data search
    \       escape the character right after"
  (interactive "P")
  (when select-dictionary-list
    (if (not sdcv-dictionary-alist)
        (error "ERROR: no `sdcv-dictionary-alist' defined.")
      ;; select sdcv-dictionary-list
      (setq sdcv-dictionary-list
            (cdr (assoc
                  (completing-read "Select dictionary list: "
                                   sdcv-dictionary-alist nil t)
                  sdcv-dictionary-alist)))
      ;; kill sdcv process
      (and (get-process sdcv-process-name)
           (kill-process (get-process sdcv-process-name)))))
  (let ((word (if (and transient-mark-mode mark-active)
                  (buffer-substring-no-properties (region-beginning)
                                                  (region-end))
                (sdcv-current-word))))
    (setq word (read-string
                (format "Search the dictionary for (default %s): " word)
                nil nil word))
    (sdcv-search-word word)))

(defun sdcv-search-word (word)
  "Search WORD through the command-line tool sdcv.
The result will be displayed in buffer named with
`sdcv-buffer-name' with `sdcv-mode'."
  (with-current-buffer (get-buffer-create sdcv-buffer-name)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (sdcv-do-lookup word)))
  (sdcv-goto-sdcv)
  (sdcv-mode)
  (sdcv-mode-reinit))

(defun sdcv-list-dictionary ()
  "Show available dictionaries."
  (interactive)
  (let (resize-mini-windows)
    (shell-command "sdcv -l" sdcv-buffer-name)))

(defun sdcv-generate-dictionary-argument ()
  "Generate dictionary argument for sdcv from `sdcv-dictionary-list'
and `sdcv-dictionary-path'."
  (append
   (if (null sdcv-dictionary-path)
       '()
     (list "--data-dir" sdcv-dictionary-path))
   (if (null sdcv-dictionary-list)
       '()
     (mapcan (lambda (dict)
               (list "-u" dict))
             sdcv-dictionary-list))))

;;; ==================================================================
;;; utilities to switch from and to sdcv buffer
(defun sdcv-current-word ()
  "Get the current word under the cursor."
  (if (or (< emacs-major-version 21)
          (and (= emacs-major-version 21)
               (< emacs-minor-version 4)))
      (sdcv-current-word-1)
    ;; We have a powerful `current-word' function since 21.4
    (current-word nil t)))
(defun sdcv-current-word-1 ()
  (save-excursion
    (backward-word 1)
    (mark-word 1)
    (buffer-substring-no-properties (region-beginning)
                                    (region-end))))
(defvar sdcv-previous-window-conf nil
  "Window configuration before switching to sdcv buffer.")
(defun sdcv-goto-sdcv ()
  "Switch to sdcv buffer in other window."
  (interactive)
  (unless (eq (current-buffer)
	      (sdcv-get-buffer))
    (setq sdcv-previous-window-conf (current-window-configuration)))
  (let* ((buffer (sdcv-get-buffer))
         (window (get-buffer-window buffer)))
    (if (null window)
        (switch-to-buffer-other-window buffer)
      (select-window window))))
(defun sdcv-return-from-sdcv ()
  "Bury sdcv buffer and restore the previous window configuration."
  (interactive)
  (if (window-configuration-p sdcv-previous-window-conf)
      (progn
        (set-window-configuration sdcv-previous-window-conf)
        (setq sdcv-previous-window-conf nil)
        (bury-buffer (sdcv-get-buffer)))
    (bury-buffer)))

(defun sdcv-get-buffer ()
  "Get the sdcv buffer. Create one if there's none."
  (let ((buffer (get-buffer-create sdcv-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'sdcv-mode)
        (sdcv-mode)))
    buffer))

;;; ==================================================================
;;; The very major mode
(defvar sdcv-mode-font-lock-keywords
  '(
    ;; dictionary name
    ("^-->\\(.*\\)$" . (1 font-lock-type-face))
    ;; property of word
    ("^<<\\([^>]*\\)>>$" . (1 font-lock-comment-face))
    ;; phonetic symbol
    ("^\\[\\([^]]*\\)\\]$" . (1 font-lock-string-face))
    )
  "Expressions to hilight in `sdcv-mode'")

(defvar sdcv-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'sdcv-return-from-sdcv)
    (define-key map "s" 'isearch-forward-regexp)
    (define-key map "r" 'isearch-backward-regexp)
    (define-key map (kbd "C-s") 'isearch-forward)
    (define-key map (kbd "C-r") 'isearch-backward)
    (define-key map (kbd "RET") 'sdcv-mode-scroll-up-one-line)
    (define-key map (kbd "M-RET") 'sdcv-mode-scroll-down-one-line)
    (define-key map "v" 'scroll-up)
    (define-key map (kbd "M-v") 'scroll-down)
    (define-key map " " 'scroll-up)
    (define-key map (kbd "DEL") 'scroll-down)
    (define-key map (kbd "C-n") 'sdcv-mode-next-line)
    (define-key map "n" 'sdcv-mode-next-line)
    (define-key map (kbd "C-p") 'sdcv-mode-previous-line)
    (define-key map "p" 'sdcv-mode-previous-line)
    (define-key map "d" 'sdcv-search)
    (define-key map "?" 'describe-mode)
    (define-key map "a" 'show-all)
    (define-key map "h" 'hide-body)
    (define-key map "e" 'show-entry)
    (define-key map "c" 'hide-entry)
    map)
  "Keymap for `sdcv-mode'.")

(define-derived-mode sdcv-mode nil "sdcv"
  "Major mode to look up word through sdcv.
\\{sdcv-mode-map}
Turning on Text mode runs the normal hook `sdcv-mode-hook'."
  (setq font-lock-defaults '(sdcv-mode-font-lock-keywords))
  (setq buffer-read-only t)
  (set (make-local-variable 'outline-regexp) "^-->.*\n-->"))

(defun sdcv-mode-reinit ()
  "Re-initialize buffer.
Hide all entrys but the first one and goto
the beginning of the buffer."
  (setq buffer-read-only nil)
  ;; ;; replace "-->...\n-->" to "-->...: "
  ;; (goto-char (point-min))
  ;; (while (re-search-forward "-->\\(.*\\)\\(\n-->\\)" nil t)
  ;;   (replace-match "-->\\1: " t))

  ;; ;; hilight word
  ;; (hi-lock-mode 0)
  ;; (let ((pattern sdcv-result-patterns)
  ;;       done)
  ;;   (while (and pattern (not done))
  ;;     (goto-char (point-min))
  ;;     (when (re-search-forward (car pattern) nil t)
  ;;       (highlight-regexp (match-string 1) 'font-lock-keyword-face)
  ;;       (setq done t))
  ;;     (setq pattern (cdr pattern))))

  (ignore-errors
    (setq buffer-read-only t)
    (hide-body)
    (goto-char (point-min))
    (next-line 1)
    (show-entry)))

(defun sdcv-mode-scroll-up-one-line ()
  (interactive)
  (scroll-up 1))
(defun sdcv-mode-scroll-down-one-line ()
  (interactive)
  (scroll-down 1))
(defun sdcv-mode-next-line ()
  (interactive)
  (ignore-errors
    (next-line 1)
    (save-excursion
      (beginning-of-line nil)
      (when (looking-at outline-regexp)
        (show-entry)))))
;; I decide not to fold the definition entry when
;; doing previous-line. So `sdcv-mode-previous-line'
;; is only an alias of `previous-line'.
(defalias 'sdcv-mode-previous-line 'previous-line)

;;; ==================================================================
;;; Support for sdcv process in background
(defun sdcv-do-lookup (word)
  "Send the word to the sdcv process and return the result."
  (let ((process (sdcv-get-process)))
    (process-send-string process (concat word "\n"))
    (with-current-buffer (process-buffer process)
      (let ((i 0) rlt done)
	(while (and (not done)
		    (< i sdcv-wait-timeout))
	  (when (sdcv-match-tail sdcv-word-prompts)
	    (setq rlt (buffer-substring-no-properties (point-min)
						      (point-max)))
	    (setq done t))
	  (when (sdcv-match-tail sdcv-choice-prompts)
	    (process-send-string process "-1\n"))
	  (unless done
	    (sleep-for sdcv-wait-interval)
	    (setq i (+ i sdcv-wait-interval))))
	(unless (< i sdcv-wait-timeout)
	  ;; timeout
	  (kill-process process)
	  (error "ERROR: timeout waiting for sdcv"))
	(erase-buffer)
	rlt))))

(defvar sdcv-wait-timeout 2
  "The max time (in seconds) to wait for the sdcv process to
produce some output.")
(defvar sdcv-wait-interval 0.01
  "The interval (in seconds) to sleep each time to wait for
sdcv's output.")

(defconst sdcv-process-name "%sdcv-mode-process%")
(defconst sdcv-process-buffer-name "*sdcv-mode-process*")

(defvar sdcv-word-prompts '("Enter word or phrase: "
			    "请输入单词或短语："
			    "請輸入單字或片語：")
  "A list of prompts that sdcv use to prompt for word.")

(defvar sdcv-choice-prompts '("Your choice[-1 to abort]: "
			      "您的选择为："
			      "您的選擇為：")
  "A list of prompts that sdcv use to prompt for a choice
of multiple candicates.")

(defvar sdcv-result-patterns '("^Found [0-9]+ items, similar to [*?/|]*\\(.+?\\)[*?]*\\."
			      "^发现 [0-9]+ 条记录和 [*?/|]*\\(.+?\\)[*?]* 相似。"
			      )
  "A list of patterns to extract result word of sdcv. Special
characters are stripped.")

(defun sdcv-get-process ()
  "Get or create the sdcv process."
  (let ((process (get-process sdcv-process-name)))
    (when (null process)
      (with-current-buffer (get-buffer-create
			    sdcv-process-buffer-name)
	(erase-buffer)
	(setq process (apply 'start-process
			     sdcv-process-name
			     sdcv-process-buffer-name
			     sdcv-program-path
			     (sdcv-generate-dictionary-argument)))
	;; kill the initial prompt
	(let ((i 0))
	  (message "starting sdcv...")
	  (while (and (not (sdcv-match-tail sdcv-word-prompts))
		      (< i sdcv-wait-timeout))
	    (sleep-for sdcv-wait-interval)
	    (setq i (+ i sdcv-wait-interval)))
	  (unless (< i sdcv-wait-timeout)
	    ;; timeout
	    (kill-process process)
	    (error "ERROR: timeout waiting for sdcv"))
	  (erase-buffer))))
    process))

(defun sdcv-buffer-tail (length)
  "Get a substring of length LENGTH at the end of
current buffer."
  (let ((beg (- (point-max) length))
	(end (point-max)))
    (if (< beg (point-min))
	(setq beg (point-min)))
    (buffer-substring-no-properties beg end)))

(defun sdcv-match-tail (prompts)
  (let ((done nil)
	(prompt nil))
    (while (and (not done)
		prompts)
      (setq prompt (car prompts))
      (setq prompts (cdr prompts))
      (when (string-equal prompt
                          (sdcv-buffer-tail (length prompt)))
        (delete-region (- (point-max) (length prompt))
                       (point-max))
        (setq done t)))
    done))


;;;;##################################################################
;;;;  User Options, Variables
;;;;##################################################################

(defvar sdcv-buffer-name "*sdcv*"
  "The name of the buffer of sdcv.")
(defvar sdcv-dictionary-list nil
  "A list of dictionaries to use.
Each entry is a string denoting the name of a dictionary, which
is then passed to sdcv through the '-u' command line option. If
this list is nil then all the dictionaries will be used.")
(defvar sdcv-dictionary-alist nil
  "An alist of dictionaries, used to interactively form
 sdcv-dictionary-list. It has the form:
   ((\"group1\" \"dict1\" \"dict2\" ...)
    (\"group2\" \"dict2\" \"dict3\"))
")

(defvar sdcv-program-path "sdcv"
  "The path of sdcv program.")

(defvar sdcv-dictionary-path nil
  "The path of dictionaries.")
;;; sdcv-mode.el ends here
