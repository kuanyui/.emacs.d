;;; rc-shell.el ---                                  -*- lexical-binding: t; -*-

;;======================================================
;; shell-script-mode
;;======================================================

;;較完整地支援shell script語法高亮。
;; (defface font-lock-system-command-face
;; '((((class color)) (:foreground "purple")))
;; "I am comment"
;; :group 'font-lock-faces)

;; (defun font-lock-system-command (&optional limit)
;; ""
;; (and (search-forward-regexp "\\<[a-zA-Z\\-]+\\>" limit t)
;;      (executable-find
;;       (buffer-substring-no-properties (car (bounds-of-thing-at-point 'word))
;;                                       (cdr (bounds-of-thing-at-point 'word))))))
;;
;; (font-lock-add-keywords 'sh-mode
;;                         '((font-lock-system-command . 'font-lock-system-command-face)))

;; One-key to run script with Bash
;; (require 'sh-script)
(with-eval-after-load 'sh-script
  (define-key sh-mode-map (kbd "<f5>") 'run-current-sh)
  (defun run-current-sh ()
    (interactive)
    (save-buffer)(shell-command (format "bash %s" (buffer-real-name))))
  )

(defun my/sh-extra-font-lock ()
  "Beef up shell-script-mode highlighting."
  (let ((builtin-keywords (regexp-opt '("declare" "local" "readonly" "export" "unset" "typeset" "shift" "trap" "set" "eval" "exec" "source" "alias" "unalias") 'symbols)))
    (font-lock-add-keywords
     nil
     `(("\\(\\$[A-Za-z_][A-Za-z0-9_]*\\)" 1 font-lock-variable-name-face t) ; $VAR (also inside strings)
       ("\\(\\${[^}]+}\\)" 1 font-lock-variable-name-face t) ; ${VAR}, ${VAR:-default}, etc.
       ("\\(\\$[0-9@*#?$!-]\\)" 1 font-lock-variable-name-face t) ; positional/special params: $1 $@ $# $? $$ $!
       ("\\(\\$(\\|)\\)" 1 font-lock-preprocessor-face t) ; $(...) command substitution delimiters
       ("`\\([^`]*\\)`" 0 font-lock-preprocessor-face t) ; backtick command substitution
       ("\\([<>]&?[0-9]*\\|[0-9]*>>?\\|<<<?\\)" 1 font-lock-warning-face) ; redirection operators
       (,builtin-keywords . font-lock-builtin-face) ; common builtins
       ))))

(add-hook 'sh-mode-hook #'my/sh-extra-font-lock)
(add-hook 'shell-script-mode #'my/sh-extra-font-lock)
(add-hook 'bash-ts-mode-hook #'my/sh-extra-font-lock)

(provide 'rc-shell)
;;; rc-shell.el ends here
