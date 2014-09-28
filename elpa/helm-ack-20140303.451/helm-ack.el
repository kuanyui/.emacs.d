;;; helm-ack.el --- Ack command with helm interface

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-ack
;; Version: 20140303.451
;; X-Original-Version: 0.08
;; Package-Requires: ((helm "1.0") (cl-lib "0.5"))

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

;;; Code:

(require 'cl-lib)

(require 'helm)

(defgroup helm-ack nil
  "Ack command with helm interface"
  :group 'helm)

(defcustom helm-c-ack-use-ack-grep nil
  "Use ack-grep command"
  :type 'boolean
  :group 'helm-ack)

(defcustom helm-c-ack-base-command "ack -H --nocolor --nogroup"
  "Base command of `ack'"
  :type 'string
  :group 'helm-ack)

(defcustom helm-c-ack-auto-set-filetype t
  "Setting file type automatically"
  :type 'boolean
  :group 'helm-ack)

(defcustom helm-c-ack-version nil
  "ack version"
  :type 'integer
  :group 'helm-ack)

(defcustom helm-c-ack-insert-at-point 'word
  "Insert thing at point as search pattern.
   You can set value same as `thing-at-point'"
  :type 'symbol
  :group 'helm-ack)

(defvar helm-c-ack-context-stack nil
  "Stack for returning the point before jump")

(defun helm-c-ack-mode-to-type (mode)
  (cl-case mode
    (actionscript-mode "ack")
    (ada-mode "ada")
    (asm-mode "asm")
    (batch-mode "batch")
    (c-mode "cc")
    (clojure-mode "clojure")
    (c++-mode "cpp")
    (csharp-mode "csharp")
    (css-mode "css")
    (emacs-lisp-mode "elisp")
    (erlang-mode "erlang")
    ((fortan-mode f90-mode) "fortran")
    (go-mode "go")
    (groovy-mode "groovy")
    (haskell-mode "haskell")
    (html-mode "html")
    (java-mode "java")
    ((javascript-mode js-mode js2-mode) "js")
    (lisp-mode "lisp")
    (lua-mode "lua")
    (makefile-mode "make")
    (objc-mode "objc")
    ((ocaml-mode tuareg-mode) "ocaml")
    ((perl-mode cperl-mode) "perl")
    (php-mode "php")
    (python-mode "python")
    (ruby-mode "ruby")
    (scala-mode "scala")
    (scheme-mode "scheme")
    (shell-script-mode "shell")
    (sql-mode "sql")
    (tcl-mode "tcl")
    ((tex-mode latex-mode yatex-mode) "tex")))

(defsubst helm-c-ack-all-type-option ()
  (if (= helm-c-ack-version 1)
      "--all"
    ""))

(defun helm-c-ack-type-option ()
  (let ((type (helm-c-ack-mode-to-type major-mode)))
    (if type
        (format "--type=%s" type)
      (helm-c-ack-all-type-option))))

(defun helm-c-ack-thing-at-point ()
  (let ((str (thing-at-point helm-c-ack-insert-at-point)))
    (if (and str (typep str 'string))
        (substring-no-properties str)
      "")))

(defun helm-c-ack-default-pattern ()
  (if (null helm-c-ack-insert-at-point)
      ""
    (helm-c-ack-thing-at-point)))

(defun helm-c-ack-init-command ()
  (format "%s %s %s"
          (if helm-c-ack-use-ack-grep
              (replace-regexp-in-string "\\`ack" "ack-grep" helm-c-ack-base-command)
            helm-c-ack-base-command)
          (or (and helm-c-ack-auto-set-filetype (helm-c-ack-type-option)) "")
          (helm-c-ack-default-pattern)))

(defun helm-c-ack-save-current-context ()
  (let ((file (buffer-file-name helm-current-buffer))
        (curpoint (with-current-buffer helm-current-buffer
                    (point))))
    (push `((file  . ,file)
            (point . ,curpoint)) helm-c-ack-context-stack)))

;;;###autoload
(defun helm-ack-pop-stack ()
  (interactive)
  (let ((context (pop helm-c-ack-context-stack)))
    (unless context
      (error "Context stack is empty!!"))
    (let ((file (assoc-default 'file context))
          (curpoint (assoc-default 'point context)))
      (find-file file)
      (goto-char curpoint))))

(defvar helm-c-ack-command-stack nil
  "Command history stack for helm-ack")

(defun helm-c-ack-placeholders ()
  (cond ((buffer-file-name) `(("\\$\\$" . ,(file-name-nondirectory
                                            (buffer-file-name)))))
        (dired-directory `(("\\$\\$" . ,dired-directory)))))

(defun helm-c-ack-replace-placeholder (cmd)
  (cl-loop with replaced = (copy-sequence cmd)
           for (holder . value) in (helm-c-ack-placeholders)
           do
           (setq replaced (replace-regexp-in-string holder value replaced))
           finally return replaced))

(defun helm-c-set-ack-version ()
  (let* ((ack-cmd (if helm-c-ack-use-ack-grep "ack-grep" "ack"))
         (version-cmd (concat ack-cmd " --version"))
         (check-regexp (concat "^" ack-cmd " \\([0-9]+\\)\.[0-9]+")))
    (with-temp-buffer
      (unless (zerop (call-process-shell-command version-cmd nil t))
        (error "Failed: %s --version" ack-cmd))
      (goto-char (point-min))
      (if (re-search-forward check-regexp nil t)
          (setq helm-c-ack-version (string-to-number (match-string 1)))
        (error "Failed: ack version not found. Please set explicitly")))))

(defun helm-c-ack-init ()
  (unless helm-c-ack-version
    (helm-c-set-ack-version))
  (let ((cmd (read-string "Command: "
                          (helm-c-ack-init-command)
                          'helm-c-ack-command-stack)))
    (helm-attrset 'recenter t)
    (helm-attrset 'before-jump-hook 'helm-c-ack-save-current-context)
    (let ((buf-coding buffer-file-coding-system)
          (filled (with-helm-current-buffer
                    (helm-c-ack-replace-placeholder cmd))))
      (with-current-buffer (helm-candidate-buffer 'global)
        (let* ((coding-system-for-read buf-coding)
               (coding-system-for-write buf-coding)
               (ret (call-process-shell-command filled nil t)))
          (cond ((= ret 1) (error "no match"))
                ((not (= ret 0)) (error "Failed ack"))))))))

(defun helm-c-ack-source (arg)
  `((name . ,(if (< arg 0)
                 (format "Ack Seach(Only %s)"
                         (file-name-nondirectory (buffer-file-name)))
               "Ack Search"))
    (init . helm-c-ack-init)
    (candidates-in-buffer)
    (type . file-line)
    (candidate-number-limit . 9999)))

;;;###autoload
(defun helm-ack (arg)
  (interactive "p")
  (let ((buf (get-buffer-create "*helm ack*")))
    (helm-other-buffer (helm-c-ack-source arg) buf)))

(provide 'helm-ack)

;;; helm-ack.el ends here
