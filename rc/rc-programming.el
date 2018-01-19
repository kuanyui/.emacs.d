;;; rc-programming.el ---                                   -*- lexical-binding: t; -*-

;; Highlight \n, %s...etc
;; This will cause highlight-symbol corrupt!
;; (add-hook 'prog-mode-hook
;;        '(lambda ()
;;           (highlight-regexp "%[[:alpha:]]\\|\\\\[[:alpha:]]" 'font-lock-constant-face)
;;           (highlight-regexp "\\[\\(TODO\\|FIXME\\)\\]" 'org-todo)))

;; Try another way...:
;;(font-lock-add-keywords 'js-mode '(("\\(TODO\\|FIXME\\)" 0 'font-lock-warning-face prepend)))

;; ======================================================
;; Company
;; ======================================================

(autoload 'company-mode "company" nil t)
(setq company-minimum-prefix-length 1)
(setq company-idle-delay 0.3)

(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook 'company-mode)

;;======================================================
;; Highlight-symbol
;;======================================================

(require 'highlight-symbol)
(define-key prog-mode-map (kbd "C-c M-n") 'highlight-symbol-at-point)
(define-key prog-mode-map (kbd "M-n")'highlight-symbol-next)
(define-key prog-mode-map (kbd "M-p")'highlight-symbol-prev)
(define-key prog-mode-map (kbd "C-c M-p") 'highlight-symbol-query-replace)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)
(setq highlight-symbol-idle-delay 1.0)

;; ======================================================
;; Which Func
;; ======================================================

;; ======================================================
;; Editor Config
;; ======================================================
(require 'editorconfig)
(defun enable-editorconfig-mode ()
  (editorconfig-mode 1))

(add-hook 'prog-mode-hook #'enable-editorconfig-mode)
(add-hook 'html-mode-hook #'enable-editorconfig-mode)
(add-hook 'css-mode-hook #'enable-editorconfig-mode)

;; ======================================================
;; CSS / Stylus keymap
;; ======================================================

(mapc
 (lambda (name)
   (let ((mode-symbol      (intern (concat name "-mode")))
         (mode-hook-symbol (intern (concat name "-mode-hook")))
         (mode-map-symbol  (intern (concat name "-mode-map"))))
     (eval-after-load mode-symbol
       `(progn
          (add-hook   (quote ,mode-hook-symbol) 'highlight-symbol-mode)
          (define-key ,mode-map-symbol (kbd "C-c M-n") 'highlight-symbol-at-point)
          (define-key ,mode-map-symbol (kbd "M-n")'highlight-symbol-next)
          (define-key ,mode-map-symbol (kbd "M-p")'highlight-symbol-prev)
          (define-key ,mode-map-symbol (kbd "C-c M-p") 'highlight-symbol-query-replace))))
   )
 '("css" "stylus" "jade" "yajade"
   "conf" "conf-colon"
   "c++" "c" "java"
   "qml"
   "prog"))

;; ======================================================
;; imenu
;; ======================================================
;;(setq imenu-use-popup-menu t)

(define-key prog-mode-map (kbd "C-x i") 'helm-imenu)

;;======================================================
;; CamelCase
;;======================================================

;; 自動斷開camelCase
(add-hook 'prog-mode-hook (lambda () (subword-mode 1)))
(global-subword-mode t)
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
             lisp-interaction-mode-hook
             ))
  (add-hook x
            (lambda ()
              (rainbow-delimiters-mode t)
              (setq show-trailing-whitespace t))))

;;======================================================
;; Rainbow-mode 自動顯示色碼顏色，如 #ffeeaa
;;======================================================
(require 'rainbow-mode)
(setq rainbow-html-colors t)  ;; highlight hsl()
(global-set-key (kbd "C-x r a") 'rainbow-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)
(setq rainbow-ansi-colors nil)

;; CSS and Rainbow modes
(defun all-css-modes() (css-mode) (rainbow-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . all-css-modes)) ;; Load both major and minor modes in one call based on file type

(defun my-xml-mode () (rainbow-mode 1) (xml-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . my-xml-mode))

(add-hook 'nxml-mode-hook #'my-xml-mode)



;;======================================================
;; mmm-mode
;;======================================================

(require 'mmm-mode)
(require 'mmm-auto)
(setq mmm-global-mode 'maybe)


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
(defun my-whitespace-cleanup ()
  (interactive)
  (if (null whitespace-style)
      (message "Saved without my-whitespace-cleanup!")
    (let ((whitespace-style '(empty trailing)))
      (whitespace-cleanup)
      (if (string-suffix-p ".jade" (buffer-name))
          (save-excursion
            (goto-char (point-max))
            (insert "\n\n\n")
            )))))

(remove-hook 'before-save-hook 'my-whitespace-cleanup)
(remove-hook 'after-save-hook 'swoop-cache-clear)
(add-hook 'before-save-hook 'my-whitespace-cleanup)

;;======================================================
;; Helm-dash - Looking up documents
;;======================================================
(require 'helm-dash)
(setq helm-dash-use-curl-and-wget t)
(setq helm-dash-common-docsets '("Python 3" "Qt" "Django" "jQuery"))
(global-set-key (kbd "C-c d d") 'helm-dash)

(defmacro helm-dash-generate-doc-function(name-string)
  (let ((name-symbol (replace-regexp-in-string "[_ ]" "-" (downcase name-string))))
    `(defalias (quote ,(intern (concat "dash:" name-symbol)))
       (function (lambda ()
                   (interactive)
                   (let ((helm-dash-common-docsets (quote (,name-string))))
                     (helm-dash)))))))

(helm-dash-generate-doc-function "Python 3")
(helm-dash-generate-doc-function "Qt")
(helm-dash-generate-doc-function "Django")
(helm-dash-generate-doc-function "jQuery")
(helm-dash-generate-doc-function "JavaScript")

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

(setq projectile-enable-caching t)
(projectile-global-mode t)
(setq projectile-indexing-method 'alien)
(setq projectile-sort-order 'recentf)  ;; [NOTICE] Not works in helm.

;;Helm integration with Projectile
(require 'helm-projectile)
(helm-projectile-off)
;; (helm-projectile-on)  // I just want to use its ag/ack/grep/recentf support
(add-hook 'projectile-mode-hook
          (lambda ()
            (define-key projectile-mode-map [remap projectile-recentf] #'helm-projectile-recentf)
            (define-key projectile-mode-map [remap projectile-switch-to-buffer] #'helm-projectile-switch-to-buffer)
            (define-key projectile-mode-map [remap projectile-grep] #'helm-projectile-grep)
            (define-key projectile-mode-map [remap projectile-ack] #'helm-projectile-ack)
            (define-key projectile-mode-map [remap projectile-ag] #'helm-projectile-ag)))


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

;; ======================================================
;; wgrep
;; ======================================================
(setq wgrep-auto-save-buffer t)
(setq wgrep-enable-key (kbd "C-x C-q"))
(setq wgrep-change-readonly-file t)


;; ======================================================
;; SVG
;; ======================================================
(setq svg-path-d-keyword
      '(
        ("[A-z]" (0 font-lock-builtin-face append))
        ("," (0 font-lock-constant-face append))
        ))
;;(qml--gen-font-lock-keywords '("aaa") 'font-lock-keyword-face)
(setq svg-path-d-keywords '(svg-path-d-keyword))

(define-derived-mode svg-path-d-mode fundamental-mode "<path d>"
  (setq-local font-lock-defaults (list svg-path-d-keywords)))

;; ======================================================
;; Flycheck
;; ======================================================
(setq flycheck-keymap-prefix (kbd "C-x !"))

(provide 'rc-programming)


;;; rc-programming.el ends here
