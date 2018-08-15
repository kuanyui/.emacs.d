;;; rc-web-development.el ---                        -*- lexical-binding: t; -*-

;; ======================================================
;; SCSS-mode
;; ======================================================

;;======================================================
;; Web-mode
;;======================================================

(require 'web-mode)
(define-key web-mode-map (kbd "<backtab>") 'web-mode-fold-or-unfold)

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
(require 'yajade-mode)



(defun my-stylus-mode () (stylus-mode) (rainbow-mode))
(add-to-list 'auto-mode-alist '("\\.styl$" . my-stylus-mode))
(add-to-list 'auto-mode-alist '("\\.ejs$" . web-mode))
;;(add-to-list 'auto-mode-alist '("\\.jade$" . yajade-mode))



(add-hook 'js-mode-hook 'hl-line-mode)

(require 'smart-tab)
;;(global-smart-tab-mode 1)

(defun set-tab-width-to-2 ()
  (setq-local tab-width 2))

(add-hook 'stylus-mode-hook #'set-tab-width-to-2)

(add-hook 'jade-mode-hook #'set-tab-width-to-2)

;; ======================================================
;; Jade (Pug) + Embedded CoffeeScript (MMM-mode)
;; ======================================================



(require 'coffee-mode)
(require 'less-css-mode)

(custom-set-variables '(coffee-tab-width 2))

(add-hook 'coffee-mode-hook 'rainbow-delimiters-mode)

(defvar less-css-mode-hook '())
(add-hook 'less-css-mode-hook 'rainbow-mode)


(setq whitespace-action '(auto-cleanup))
(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))

(defun ttt ()
  (interactive)
  (let ((whitespace-style '(trailing space-before-tab indentation empty space-after-tab)))
    (whitespace-cleanup)))
;;       =================== test ===================================


(require 'mmm-mode)


(mmm-add-classes
 '((mmm-ml-pug-es6-mode
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "^ *script\\.\n"
    :back "^\n\n")
   (mmm-ml-pug-coffee-mode
    :submode coffee-mode
    :face mmm-code-submode-face
    :front "^ *script\n +:coffee-script\n"
    :back "^\n\n")
   (mmm-ml-pug-es6-babel-mode
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "^ *script\n +:babel\n"
    :back "^\n\n")
   (mmm-ml-pug-css-mode
    :submode omg-scss-mode
    :face mmm-code-submode-face
    :front "^style\\.\n"
    :back "^\n\n")
   )
 )






(mmm-add-mode-ext-class 'jade-mode nil 'mmm-ml-pug-css-mode)
(mmm-add-mode-ext-class 'jade-mode nil 'mmm-ml-pug-coffee-mode)
(mmm-add-mode-ext-class 'jade-mode nil 'mmm-ml-pug-es6-mode)
(mmm-add-mode-ext-class 'jade-mode nil 'mmm-ml-pug-es6-babel-mode)

(mmm-add-mode-ext-class 'yajade-mode nil 'mmm-ml-pug-css-mode)
(mmm-add-mode-ext-class 'yajade-mode nil 'mmm-ml-pug-coffee-mode)
(mmm-add-mode-ext-class 'yajade-mode nil 'mmm-ml-pug-es6-mode)
(mmm-add-mode-ext-class 'yajade-mode nil 'mmm-ml-pug-es6-babel-mode)



;; ======================================================
;; Vue.js
;; ======================================================

;;(add-to-list 'vue-modes '(:type template :name pug :mode jade-mode))

(add-to-list 'auto-mode-alist '("\\.vue\\'" . html-mode))

(mmm-add-classes
 '((mmm-html-vue-jade-mode
    :submode yajade-mode
    :front "<template lang=[\"']\\(?:pug\\|jade\\)[\"']>"
    :back "</template>"
    :front-offset 1
    :back-offset 1
    ;; :creation-hook (lambda () (rainbow-mode -1))
    )

   (mmm-html-vue-es6-mode
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "<script[^>]*?>\n"
    :back "</script>"
    :front-offset 0
    :creation-hook (lambda () (add-keywords-for-vuejs))
    )

   (mmm-html-vue-scss-mode
    :submode less-css-mode
    :face mmm-code-submode-face
    :front "<style .*lang=[\"']scss[\"'][^>]*>\n"
    :back "</style>"
    :front-offset 0
    ))
 )


(mmm-add-mode-ext-class 'html-mode nil 'mmm-html-vue-jade-mode)
(mmm-add-mode-ext-class 'html-mode nil 'mmm-html-vue-es6-mode)
(mmm-add-mode-ext-class 'html-mode nil 'mmm-html-vue-scss-mode)
;; (add-hook 'jade-mode-hook 'rainbow-mode)

(add-hook 'css-mode-hook 'company-mode)
(add-hook 'scss-mode-hook 'company-mode)
(add-hook 'html-mode-hook 'company-mode)
(add-hook 'js-mode-hook 'company-mode)
(add-hook 'js2-mode-hook 'company-mode)

(add-hook 'css-mode-hook 'highlight-symbol-mode)
(add-hook 'scss-mode-hook 'highlight-symbol-mode)
(add-hook 'html-mode-hook 'highlight-symbol-mode)
(add-hook 'js-mode-hook 'highlight-symbol-mode)
(add-hook 'js2-mode-hook 'highlight-symbol-mode)


(defun add-keywords-for-vuejs ()
  (if (string-suffix-p ".vue" (buffer-name))
      (font-lock-add-keywords
       nil
       (mapcar (lambda (k)
                 (cons (format "\\b%s\\b" k) 'font-lock-preprocessor-face))
               '("components" "data" "computed" "props" "watch" "events" "methods"
                 "vuex" "getters" "setters" "actions" "ready"))
       )
    ))

(add-hook 'web-mode-hook 'add-keywords-for-vuejs)
(add-hook 'html-mode-hook 'add-keywords-for-vuejs)
(add-hook 'js-mode-hook 'add-keywords-for-vuejs)
(add-hook 'js2-mode-hook 'add-keywords-for-vuejs)
(add-hook 'mmm-mode-hook 'add-keywords-for-vuejs)
(add-hook 'mmm-major-mode-hook 'add-keywords-for-vuejs)


;; ======================================================
;; Shortcut to restart mmm-mode
;; ======================================================

(defun mmm-mode-restart! ()
  (interactive)
  (widen)
  (let ((ext (file-name-extension (buffer-name))))
    (cond ((string= ext "jade")
           (jade-mode))
          ((string= ext "vue")
           (html-mode))
          (t nil))
    (my-whitespace-cleanup)
    (mmm-mode-off)
    (mmm-mode-on)
    (message "mmm-mode restarted!")))

(define-key mmm-mode-map (kbd "<f6>") 'mmm-mode-restart!)

(define-key jade-mode-map (kbd "<f6>") 'mmm-mode-restart!)
(define-key html-mode-map (kbd "<f6>") 'mmm-mode-restart!)

(defun narrow-to-js ()
  (interactive)
  (cond ((string-suffix-p ".vue" (buffer-name))
         (save-excursion
           (goto-char (point-min))
           (let* ((beg (progn (re-search-forward "<script[^>]*> *" nil :no-error)
                              (right-char 1)
                              (point)))
                  (end (progn (re-search-forward "</script>" nil :no-error)
                              (left-char 9)
                              (point))))
             (narrow-to-region beg end)
             (js2-mode)
             )))
        ((string-suffix-p ".jade" (buffer-name))
         (let* ((coffee? (save-excursion (goto-char (point-min))
                                         (re-search-forward ":coffee-script" nil :no-error))))
           (save-excursion
             (goto-char (point-min))
             (if (null (re-search-forward "^ *script\. *\n" nil :no-error))
                 (progn (goto-char (point-min))
                        (re-search-forward "^ *script *\n" nil :no-error)))
             (if coffee? (re-search-forward ":coffee-script" nil :no-error))
             (let* ((beg (progn (right-char 1) (point)))
                    (end (point-max)))
               (narrow-to-region beg end)
               (if coffee? (coffee-mode) (js2-mode))
               ))))
        (t
         (message "Error, please edit `narrow-to-js'"))
        ))

(define-key mmm-mode-map (kbd "<f7>") 'narrow-to-js)

(define-key jade-mode-map (kbd "<f7>") 'narrow-to-js)
(define-key html-mode-map (kbd "<f7>") 'narrow-to-js)

(require 'js2-mode)
(define-key js2-mode-map (kbd "<f6>") 'mmm-mode-restart!)
(define-key coffee-mode-map (kbd "<f6>") 'mmm-mode-restart!)



;; ======================================================
;; Firefox Controller
;; ======================================================
(require 'firefox-controller)
(require 'nxml-mode)
(require 'highlight-symbol)
(define-key nxml-mode-map (kbd "C-c M-n") 'highlight-symbol-at-point)
(define-key nxml-mode-map (kbd "M-n")'highlight-symbol-next)
(define-key nxml-mode-map (kbd "M-p")'highlight-symbol-prev)
(define-key nxml-mode-map (kbd "C-c M-p") 'highlight-symbol-query-replace)
(add-hook 'nxml-mode-hook 'highlight-symbol-mode)
(setq highlight-symbol-idle-delay 1.0)

;; (global-set-key (kbd "<f11>") 'firefox-controller-remote-mode)

(require 'browser-f5)
(define-key web-mode-map (kbd "<f5>") 'browser-f5)
(define-key jade-mode-map (kbd "<f5>") 'browser-f5)
(define-key yajade-mode-map (kbd "<f5>") 'browser-f5)
(define-key nxml-mode-map (kbd "<f5>") 'browser-f5)


;; ======================================================
;; Some dirty shit for refactoring shitty legacy code
;; ======================================================
;;(font-lock-add-keywords 'js-mode '(("\\b[A-Z][A-z0-9]+\\b" 0 'font-lock-warning-face)
;;("\\b[A-Z][A-z0-9]+ *: *" 0 'font-lock-warning-face)
;;("['\"][A-Z][A-z0-9]+[\"']" 0 'font-lock-warning-face)
;;))
;;(font-lock-add-keywords 'coffee-mode '(("\\b[A-Z][A-z0-9]+\\b" 0 'font-lock-warning-face)
;;                                       ("\\b[A-Z][A-z0-9]+ *: *" 0 'font-lock-warning-face)
;;                                       ("['\"][A-Z][A-z0-9]+[\"']" 0 'font-lock-warning-face)
;;                                       ))
;;

;;(font-lock-add-keywords 'jade-mode '(("\\b\\.[A-Z][A-z0-9]+\\b" 0 'font-lock-warning-face)))
;;(font-lock-add-keywords 'fundamental-mode '(("\\b\\.[A-Z][A-z0-9]+\\b" 0 'font-lock-warning-face)))
;;(defun my-find-possible-shit-api ()
;;(interactive)
;;(let (case-fold-search)
;;(re-search-backward "\\(\\._?[A-Z][A-z0-9]+\\|\\b_?[A-Z][A-z0-9]+ *: *\\|['\"]_?[A-Z][A-z0-9]+[\"']\\)" nil t)
;;(message "%s" (char-to-string (char-after (+ 1 (point)))))
;;(if (and (string-match-p "[A-Z]" (char-to-string (char-after (+ 1 (point)))))
;;(string-match-p "[A-Z]" (char-to-string (char-after (+ 2 (point)))))
;;(string-match-p "[A-Z]" (char-to-string (char-after (+ 3 (point))))))
;;(my-find-possible-shit-api)
;;)
;;))

;;(define-key js-mode-map (kbd "<f4>") 'my-find-possible-shit-api)
;;(define-key coffee-mode-map (kbd "<f4>") 'my-find-possible-shit-api)
;;
;;(require 'go-mode)
;;(defun my-go-find-possible-shit-api ()
;;(interactive)
;;(let (case-fold-search)
;;(re-search-backward "`json:\"[A-Z][A-z0-9]+" nil t)
;;))
;;(define-key go-mode-map (kbd "<f4>") 'my-go-find-possible-shit-api)


(provide 'rc-web-development)
;;; rc-web-development.el ends here
