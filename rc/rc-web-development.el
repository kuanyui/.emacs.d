;;; rc-web-development.el ---                        -*- lexical-binding: t; -*-

;;======================================================
;; Web-mode
;;======================================================

(require 'web-mode)

(defun web-mode-element-close-and-indent ()
  (interactive)
  (web-mode-element-close)
  (indent-for-tab-command))

(define-key web-mode-map (kbd "C-c /") 'web-mode-element-close-and-indent)

;; If non-nil, when enter `</' , element will be closed automatically.
;; Else, use `C-c/' to do the same jog.
(setq web-mode-enable-auto-closing nil)

(setq-default indent-tabs-mode nil)
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)	; js, php...
  )
(setq-default web-mode-markup-indent-offset 2)
(setq-default web-mode-css-indent-offset 2)
(setq-default web-mode-code-indent-offset 2)	; js, php...


(add-hook 'web-mode-hook 'my-web-mode-hook)

(setq web-mode-auto-close-style 1)
(setq web-mode-tag-auto-close-style 1)
(web-mode-toggle-current-element-highlight)
(setq web-mode-enable-current-column-highlight t)
;; Auto-Complete support
(setq web-mode-ac-sources-alist
      '(("css" . (ac-source-css-property))
	("html" . (ac-source-words-in-buffer ac-source-abbrev))))

(setq web-mode-extra-snippets '(
				("django" . (
					     ("a" . "{% | %}")
					     ("%" . "{% | %}")
					     ("" . "{% | %}")
					     ("c" . "{# | #}")
					     ("#" . "{# | #}")
					     )
				 )
				)
      )

;; Django & Web-mode
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'ac-modes 'web-mode)
(setq web-mode-engines-alist
      '(
        ("django" . "\\.html\\'")
        ("erb" . "\\.ejs\\'")
        ))

(require 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)


(provide 'rc-web-development)
;;; rc-web-development.el ends here
