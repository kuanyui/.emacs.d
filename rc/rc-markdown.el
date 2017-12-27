;;; rc-markdown.el ---                               -*- lexical-binding: t; -*-

;;======================================================
;; Markdown
;;======================================================

(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))

(setq markdown-enable-math t)
(setq markdown-command "/usr/lib/node_modules/marked/bin/marked")

(require 'mmm-mode)
(setq mmm-global-mode 'maybe)

(setq mmm-parse-when-idle 't)

(defun my-mmm-markdown-auto-class (lang &optional submode)
  "Define a mmm-mode class for LANG in `markdown-mode' using SUBMODE.
If SUBMODE is not provided, use `LANG-mode' by default."
  (let ((class (intern (concat "markdown-" lang)))
        (submode (or submode (intern (concat lang "-mode"))))
        (front (concat "^```" lang "[\n\r]+"))
        (back "^```"))
    (mmm-add-classes (list (list class :submode submode :front front :back back)))
    (mmm-add-mode-ext-class 'markdown-mode nil class)
    (mmm-add-mode-ext-class 'gfm-mode nil class)
    ))

;; Mode names that derive directly from the language name
(mapc 'my-mmm-markdown-auto-class
      '("awk" "bibtex" "c" "cpp" "css" "html" "latex" "lisp" "makefile"
        "markdown" "python" "r" "ruby" "sql" "stata" "xml" "js"))

(defalias 'cpp-mode 'c++-mode)
;; [TODO] 把markdown的outline搞得跟org-mode的key-binding接近一點

(provide 'rc-markdown)
;;; rc-markdown.el ends here
