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
(require 'sh-script)
(define-key sh-mode-map (kbd "<f5>") 'run-current-sh)
(defun run-current-sh ()
  (interactive)
  (save-buffer)(shell-command (format "bash %s" (buffer-real-name))))


(provide 'rc-shell)
;;; rc-shell.el ends here
