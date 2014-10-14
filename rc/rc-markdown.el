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

;; [TODO] 把markdown的outline搞得跟org-mode的key-binding接近一點

(provide 'rc-markdown)
;;; rc-markdown.el ends here
