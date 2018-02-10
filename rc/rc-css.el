;;; rc-css.el ---                                    -*- lexical-binding: t; -*-

;; font-lock in SCSS-mode is buggy and may freeze the whole Emacs. Example:
;; https://gist.github.com/kuanyui/f07579d957ea2dda426b8034d78269ba
;; So derive a new scss-mode as workaround...
(define-derived-mode omg-scss-mode css-mode "OMG SCSS"
  "Major mode to edit \"Sassy CSS\" files."
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-continue " *")
  (setq-local comment-start-skip "/[*/]+[ \t]*")
  (setq-local comment-end-skip "[ \t]*\\(?:\n\\|\\*+/\\)"))

(add-to-list 'auto-mode-alist '("\\.scss$" . less-css-mode))  ;; scss-mode is suck, freezing all the time.

(provide 'rc-css)
;;; rc-css.el ends here
