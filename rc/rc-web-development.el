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
(add-to-list 'load-path "~/.emacs.d/git/emacs-pug-mode")
(require 'sws-mode)
(require 'stylus-mode)
(require 'jade-mode)
(require 'pug-mode)

(defun my-stylus-mode () (stylus-mode) (rainbow-mode))
(add-to-list 'auto-mode-alist '("\\.styl$" . my-stylus-mode))
(add-to-list 'auto-mode-alist '("\\.ejs$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

(require 'smart-tab)
;;(global-smart-tab-mode 1)

(defun set-tab-width-to-2 ()
  (setq-local tab-width 2))

(add-hook 'stylus-mode-hook #'set-tab-width-to-2)
(add-hook 'pug-mode-hook #'set-tab-width-to-2)
(add-hook 'jade-mode-hook #'set-tab-width-to-2)

;; ======================================================
;; Jade (Pug) + Embedded CoffeeScript (MMM-mode)
;; ======================================================

(add-to-list 'load-path "~/.emacs.d/git/emacs-pug-mode")
(require 'pug-mode)
(require 'coffee-mode)
(require 'less-css-mode)

(custom-set-variables '(coffee-tab-width 2))
(add-hook 'pug-mode-hook 'rainbow-delimiters-mode)
(add-hook 'coffee-mode-hook 'rainbow-delimiters-mode)

(defvar less-css-mode-hook '())
(add-hook 'less-css-mode-hook 'rainbow-mode)


(setq whitespace-action '(auto-cleanup))
(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))


(require 'mmm-mode)
(defun mmm-mode-restart! ()
  (interactive)
  (let ((ext (file-name-extension (buffer-name))))
    (cond ((string= ext "jade")
           (jade-mode))
          ((string= ext "vue")
           (html-mode))
          (t nil))
    (mmm-mode-off)
    (mmm-mode-on)
    (message "mmm-mode restarted!")))
(define-key mmm-mode-map (kbd "<f5>") 'mmm-mode-restart!)
(define-key pug-mode-map (kbd "<f5>") 'mmm-mode-restart!)
(define-key html-mode-map (kbd "<f5>") 'mmm-mode-restart!)
(define-key jade-mode-map (kbd "<f5>") 'mmm-mode-restart!)

(mmm-add-classes
 '((mmm-ml-pug-coffee-mode
    :submode coffee-mode
    :face mmm-code-submode-face
    :front ":coffee-script\n"
    :back "^\n\n")))

(mmm-add-classes
 '((mmm-ml-pug-es6-mode
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "^ *script\\.\n"
    :back "^\n\n")))

(mmm-add-mode-ext-class 'pug-mode nil 'mmm-ml-pug-css-mode)
(mmm-add-mode-ext-class 'pug-mode nil 'mmm-ml-pug-coffee-mode)
(mmm-add-mode-ext-class 'pug-mode nil 'mmm-ml-pug-es6-mode)


;; ======================================================
;; Vue.js
;; ======================================================

;;(add-to-list 'vue-modes '(:type template :name pug :mode jade-mode))

(add-to-list 'auto-mode-alist '("\\.vue\\'" . html-mode))

(mmm-add-classes
 '((mmm-html-vue-pug-mode
    :submode pug-mode
    :face mmm-code-submode-face
    :front "<template lang=\"pug\">\n"
    :back "</template>"
    :front-offset 0
    )

   (mmm-html-vue-es6-mode
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "<script>\n"
    :back "</script>"
    :front-offset 0
    )

   (mmm-html-vue-scss-mode
    :submode scss-mode
    :face mmm-code-submode-face
    :front "<style lang=[\"']scss[\"'] rel=[\"']stylesheet/scss[\"']>\n"
    :back "</style>"
    :front-offset 0
    ))
 )

(mmm-add-mode-ext-class 'html-mode nil 'mmm-html-vue-pug-mode)
(mmm-add-mode-ext-class 'html-mode nil 'mmm-html-vue-es6-mode)
(mmm-add-mode-ext-class 'html-mode nil 'mmm-html-vue-scss-mode)

(mmm-add-mode-ext-class 'jade-mode nil 'mmm-ml-pug-css-mode)
(mmm-add-mode-ext-class 'jade-mode nil 'mmm-ml-pug-coffee-mode)
(mmm-add-mode-ext-class 'jade-mode nil 'mmm-ml-pug-es6-mode)


(provide 'rc-web-development)
;;; rc-web-development.el ends here
