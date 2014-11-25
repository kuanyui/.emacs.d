;;; rc-lisp.el ---                                   -*- lexical-binding: t; -*-
;; 很三八的把Lisp程式碼中的lambda顯示成λ
(global-prettify-symbols-mode 1)

;;======================================================
;; SLIME
;;======================================================
(require 'slime-autoloads)
(require 'slime)
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
;; Optionally, specify the lisp program you are using. Default is "lisp"
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-lisp-implementations
      '((sbcl ("/usr/bin/sbcl") :coding-system utf-8-unix)))
(setq slime-net-coding-system 'utf-8-unix)
(setq slime-contribs '(slime-fancy))
(slime-setup)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
;(eval-after-load 'slime
;  `(define-key slime-prefix-map (kbd "M-h") 'slime-documentation-lookup)
;  `(define-key slime-mode-map (kbd "C-M-_") 'undo-tree-redo))
(add-hook 'lisp-mode-hook
	  (lambda ()
	    (define-key lisp-mode-map (kbd "M-_") 'undo-tree-redo)))
;;(defun keyboard-quit-custom ()
;;  (interactive)
;;  (lazy-highlight-cleanup)(keyboard-quit))
;;(global-set-key (kbd "C-g") 'keyboard-quit-custom)

(define-key lisp-mode-map (kbd "ESC <f12>")'highlight-symbol-next)
(define-key lisp-mode-map (kbd "ESC <f11>")'highlight-symbol-prev)
(add-hook 'lisp-mode-hook 'highlight-symbol-mode)

;;======================================================
;; Emacs Lisp 相關加強
;;======================================================
;;
;; 超混亂lisp的function highlight
;;
;;(defvar font-lock-func-face
;;  (defface font-lock-func-face
;;      '((nil (:weight bold))
;;        (t (:bold t :italic t)))
;;    "Font Lock mode face used for function calls."
;;    :group 'font-lock-highlighting-faces))
(font-lock-add-keywords 'emacs-lisp-mode
                        '(
                          ("#?'[-a-zA-Z_][-a-zA-Z0-9_:/+]*" 0 'font-lock-constant-face)
                          ("(\\([-a-zA-Z0-9_/]+\\)" 1 'font-lock-keyword-face)
                          ("(setq \\([-a-zA-Z0-9_/]+\\)" 1 'font-lock-variable-name-face)))

(define-key emacs-lisp-mode-map (kbd "C-h 1") 'lookup-elisp-function-doc)
(define-key emacs-lisp-mode-map (kbd "C-h 2") 'lookup-elisp-variable-doc)
(define-key lisp-interaction-mode-map (kbd "C-h 1") 'lookup-elisp-function-doc)
(define-key lisp-interaction-mode-map (kbd "C-h 2") 'lookup-elisp-variable-doc)

;; Makes eval elisp sexp more convenient
(defun eval-elisp-sexp ()
  "Eval Elisp code at the point, and remove current s-exp
With one `C-u' prefix, insert output following an arrow"
  (interactive)
  (cond ((equal current-prefix-arg nil)      ;if no prefix
         (let ((OUTPUT (eval (preceding-sexp))))
           (kill-sexp -1)
           (insert (format "%S" OUTPUT))))
        ((equal current-prefix-arg '(4)) ;one C-u prefix
         (save-excursion
           (let ((OUTPUT (eval (preceding-sexp))))
             (insert (format "%s%S" " => " OUTPUT)))))))

(global-set-key (kbd "C-c C-x C-e") 'eval-elisp-sexp)
;; avoid key-binding conflict with org
(define-key org-mode-map (kbd "C-c C-x C-e") 'org-clock-modify-effort-estimate)


(provide 'rc-lisp)
;;; rc-lisp.el ends here
