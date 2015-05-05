;;; rc-programming.el ---                                   -*- lexical-binding: t; -*-

;; Highlight \n, %s...etc
;; This will cause highlight-symbol corrupt!
;; (add-hook 'prog-mode-hook
;; 	  '(lambda ()
;; 	     (highlight-regexp "%[[:alpha:]]\\|\\\\[[:alpha:]]" 'font-lock-constant-face)
;; 	     (highlight-regexp "\\[\\(TODO\\|FIXME\\)\\]" 'org-todo)))

;;======================================================
;; Auto-complete
;;======================================================
(add-to-list 'load-path "~/.emacs.d/lisps/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-user-dictionary-files "~/.emacs.d/ac-dict") ;;我原本只有放user這個
;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict") ; 原本沒有
(ac-config-default)

(global-auto-complete-mode 1)


(define-key ac-mode-map (kbd "C-c h") 'ac-last-quick-help)
(define-key ac-mode-map (kbd "C-c H") 'ac-last-help)

;;(require 'ac-company)
;;(ac-company-define-source ac-source-company-elisp company-elisp)
;;(add-hook 'emacs-lisp-mode-hook
;;       (lambda ()
;;         (add-to-list 'ac-sources 'ac-source-company-elisp)))

(add-hook 'css-mode-hook 'ac-css-mode-setup)

(setq ac-use-menu-map t)
;; 讓C-s可以在auto-complete選單裡使用。
(define-key ac-complete-mode-map (kbd "C-s") 'ac-isearch)
(define-key ac-complete-mode-map (kbd "M-p") 'ac-quick-help-scroll-up)
(define-key ac-complete-mode-map (kbd "M-n") 'ac-quick-help-scroll-down)

(defalias 'ac 'auto-complete-mode)
;;======================================================
;; Highlight-symbol
;;======================================================

(require 'highlight-symbol)
(define-key prog-mode-map (kbd "C-c M-n") 'highlight-symbol-at-point)
(define-key prog-mode-map (kbd "M-n")'highlight-symbol-next)
(define-key prog-mode-map (kbd "M-p")'highlight-symbol-prev)
(define-key prog-mode-map (kbd "C-c M-p") 'highlight-symbol-query-replace)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)


;;======================================================
;; CamelCase
;;======================================================

;; 自動斷開camelCase
(add-hook 'prog-mode-hook (lambda () (subword-mode 1)))

;;======================================================
;; Code folding
;;======================================================

(defun my-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))
(global-set-key (kbd "C-x n f") 'my-toggle-fold)

;;======================================================
;; Smart-Operator
;;======================================================

(require 'smart-operator)
;(add-hook 'python-mode-hook 'smart-operator-mode)
(add-hook 'qml-mode-hook 'smart-operator-mode)
(add-hook 'inferior-python-mode-hook 'smart-operator-mode)
(setq smart-operator-list '("=" "<" ">" "%" "+" "-" "*" "/" "&" "|" "!" ":" "?" ","))

;;======================================================
;; Rainbow-delimiters 括號上色
;;======================================================
(require 'rainbow-delimiters)
;; 只在程式相關mode中使用
(require 'cl)
(dolist (x '(emacs-lisp-mode-hook
             lisp-mode-hook
             lisp-interaction-mode-hook))
  (add-hook x
            (lambda ()
              (rainbow-delimiters-mode t)
              (setq show-trailing-whitespace t))))

;;======================================================
;; Rainbow-mode 自動顯示色碼顏色，如 #ffeeaa
;;======================================================
(require 'rainbow-mode)
(global-set-key (kbd "C-x r a") 'rainbow-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)
(setq rainbow-ansi-colors nil)

;; CSS and Rainbow modes
(defun all-css-modes() (css-mode) (rainbow-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . all-css-modes)) ;; Load both major and minor modes in one call based on file type

(defun my-xml-mode () (rainbow-mode) (xml-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . my-xml-mode))

(require 'stylus-mode)

(defun my-stylus-mode () (stylus-mode) (rainbow-mode))
(add-to-list 'auto-mode-alist '("\\.styl$" . my-stylus-mode))
(add-to-list 'auto-mode-alist '("\\.ejs$" . web-mode))


;;======================================================
;; mmm-mode
;;======================================================

(require 'mmm-mode)
(require 'mmm-auto)
(setq mmm-global-mode 'maybe)
(mmm-add-classes
 '((mmm-ml-css-mode
    :submode css-mode
    :face mmm-code-submode-face
    :front "<style[^>]*>"
    :back "\n?[ \t]*</style>"
    )
   (mmm-ml-javascript-mode
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "<script[^>]*>[^<]"
    :front-offset -1
    :back "\n?[ \t]*</script>"
    )
   ))
(mmm-add-mode-ext-class 'html-mode nil 'mmm-ml-javascript-mode)
(mmm-add-mode-ext-class 'html-mode nil 'mmm-ml-css-mode)

(add-to-list 'auto-mode-alist '("\\.yml\\'" . conf-mode))

;;======================================================
;; Tree-mode 樹狀顯示檔案清單
;;======================================================

(require 'tree-mode)
(require 'windata)
(require 'dirtree)
(define-key dirtree-mode-map (kbd "TAB") 'tree-mode-toggle-expand)
(set-face-foreground 'widget-button "orange")

;;======================================================
;; Highlight-indentation
;;======================================================
;; (require 'highlight-indentation)
;; (add-hook 'python-mode-hook 'highlight-indentation-mode)
;; (add-hook 'python-mode-hook 'highlight-indentation-current-column-mode)
;; (set-face-background 'highlight-indentation-face "#e3e3d3")
;; (set-face-background 'highlight-indentation-current-column-face "#ffafff")
;; (setq highlight-indentation-set-offset '2)

;;======================================================
;; Whitespace
;;======================================================
(require 'whitespace)

;;======================================================
;; Helm-dash - Looking up documents
;;======================================================

(setq helm-dash-common-docsets '("Python 3" "Qt" "Django"))
(global-set-key (kbd "C-c d d") 'helm-dash)

;;======================================================
;; Aggressive-ident-mode
;;======================================================
;; http://endlessparentheses.com/aggressive-indent-just-got-better-.html
;; (global-aggressive-indent-mode)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'lisp-mode-hook #'aggressive-indent-mode)

;;======================================================
;; imenu (Emacs built-in)
;;======================================================
(define-key prog-mode-map (kbd "C-c j") 'imenu)

;;======================================================
;; Projectile
;;======================================================
(require 'ido)
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))

(defun bind-ido-keys ()
  "Keybindings for ido mode."
  (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
  (define-key ido-completion-map (kbd "<up>")   'ido-prev-match))
(add-hook 'ido-setup-hook #'bind-ido-keys)

(require 'flx-ido)
;;(ido-mode 1)
;;(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
(setq ido-enable-flex-matching t)

(setq projectile-globally-ignored-directories '("venv" "migrations"))

(projectile-global-mode t)

;;Helm integration with Projectile
(require 'helm-projectile)
(helm-projectile-on)


;;======================================================
;; Tree
;;======================================================
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)


;; ======================================================
;; Emacs-Refactor
;; ======================================================
(require 'emr-js)
(define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu)
(add-hook 'prog-mode-hook 'emr-initialize)



(provide 'rc-programming)


;;; rc-programming.el ends here
