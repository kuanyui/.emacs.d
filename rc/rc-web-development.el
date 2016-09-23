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
(setq web-mode-engines-alist
      '(
        ("django" . "\\.html\\'")
        ("erb" . "\\.ejs\\'")
        ))

(require 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)


;; ======================================================
;; Stylus / Jade <= (SWS mode) https://github.com/brianc/jade-mode
;; ======================================================

(require 'sws-mode)
(require 'stylus-mode)
(require 'jade-mode)

(defun my-stylus-mode () (stylus-mode) (rainbow-mode))
(add-to-list 'auto-mode-alist '("\\.styl$" . my-stylus-mode))
(add-to-list 'auto-mode-alist '("\\.ejs$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

(require 'smart-tab)
(global-smart-tab-mode 1)

(defun set-tab-width-to-2 ()
  (setq-local tab-width 2))

(add-hook 'stylus-mode-hook #'set-tab-width-to-2)
(add-hook 'jade-mode-hook #'set-tab-width-to-2)

;; ======================================================
;; Jade + Embedded CoffeeScript (MMM-mode)
;; ======================================================
(require 'jade-mode)
(require 'coffee-mode)
(require 'less-css-mode)

(custom-set-variables '(coffee-tab-width 2))
(add-hook 'jade-mode-hook 'rainbow-delimiters-mode)
(add-hook 'coffee-mode-hook 'rainbow-delimiters-mode)

(defvar less-css-mode-hook '())
(add-hook 'less-css-mode-hook 'rainbow-mode)


(setq whitespace-action '(auto-cleanup))
(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))


(require 'mmm-mode)
(defun mmm-mode-restart! ()
  (interactive)
  (mmm-mode-off)
  (mmm-mode-on)
  (message "mmm-mode restarted!"))
(define-key mmm-mode-map (kbd "<f5>") 'mmm-mode-restart!)
(define-key jade-mode-map (kbd "<f5>") 'mmm-mode-restart!)


(mmm-add-classes
 '((mmm-ml-jade-coffee-mode
    :submode coffee-mode
    :face mmm-code-submode-face
    :front ":coffee-script\n"
    :back "^\n$")))

(mmm-add-classes
 '((mmm-ml-jade-es6-mode
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "^ *script\\.\n"
    :front-offset 0
    :back "^\n$")))


(mmm-add-mode-ext-class 'jade-mode nil 'mmm-ml-jade-coffee-mode)
(mmm-add-mode-ext-class 'jade-mode nil 'mmm-ml-jade-es6-mode)

(provide 'rc-web-development)
;;; rc-web-development.el ends here
