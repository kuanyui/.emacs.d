;;; rc-javascript.el ---                             -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; ======================================================
;; My JavaScript config
;; ======================================================
(add-hook 'js2-mode-hook 'my-js-conf)

(defun my-js-conf ()
  (setq js2-basic-offset 2)
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-indent-switch-body t)
  (rainbow-delimiters-mode)
  (my--js-comint-conf)
  (my--js-flow-autoconf)
  (flycheck-mode 1)
  (company-mode-on)
  )

(defun my--js-comint-conf ()
  (local-set-key "\C-x\C-e" 'js-send-last-sexp)
  (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
  (local-set-key "\C-cb" 'js-send-buffer)
  (local-set-key (kbd "<f5>") 'js-run-with-shell-command)
  (local-set-key (kbd "C-x <f5>") 'js-send-buffer)
  (local-set-key "\C-c\C-l" 'js-send-buffer-and-go)
  (local-set-key "\C-cl" 'js-load-file-and-go)
  )

(defun my--js-flow-autoconf ()
  (when (flow-minor-tag-present-p)
    (js-mode)
    (flow-minor-mode)
    (message "flow detected"))
  )

;; ======================================================
;; flow
;; ======================================================
(with-eval-after-load 'flycheck
  (flycheck-add-mode 'javascript-flow 'flow-minor-mode)
  (flycheck-add-mode 'javascript-eslint 'flow-minor-mode)
  (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-flow))

;; ======================================================
;; mmm-mode
;; ======================================================
(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
(setq mmm-parse-when-idle 't)

;; Syntax-highlight For coffee-template
(font-lock-add-keywords
 'coffee-mode
 '(("['\"][0-9A-z-_]+?[\"']:" 0 'font-lock-type-face prepend) ;  'user-name':"
   ("['\"]\\.[\\.0-9A-z-_]+?[\"']" 0 'font-lock-variable-name-face prepend) ; '.class-name'
   ("['\"]\\#[0-9A-z-_]+?[\"']" 0 'font-lock-keyword-face prepend)) ; '#id'
 )
(mmm-add-classes
 '((coffee-css-mode
    :submode omg-scss-mode
    :face mmm-code-submode-face
    :front "^style +'''\n"
    :back "^'''$")))
(mmm-add-mode-ext-class 'coffee-mode nil 'coffee-css-mode)


;; ======================================================
;; Some customed highlight for js-mode
;; (Because js2-mode cannot work in mmm-mode)
;; ======================================================

;;  Highlight object keys
;; {'foo': 1}, { foo: 1 }
;; (font-lock-add-keywords 'js-mode '(("['\"][0-9A-z-_]+?[\"']:" 0 'font-lock-type-face prepend)))
;; (font-lock-add-keywords 'js-mode '(("\\b[0-9A-z]+?:" 0 'font-lock-type-face)))

;; Highlight ES6 function defining syntax
;; hello (...) { ... }
(font-lock-add-keywords 'js-mode '(("\\([0-9A-z-_]+\\) *(.*) *{" 1 'font-lock-function-name-face 'keep)) )


;; Highlight $scope for Angular
(font-lock-add-keywords 'coffee-mode '(("\\$scope" 0 'font-lock-builtin-face)))
(font-lock-add-keywords 'js-mode '(("\\$scope" 0 'font-lock-builtin-face)))
(font-lock-add-keywords 'js2-mode '(("\\$scope" 0 'font-lock-builtin-face)))

;; Highlight console.log
(font-lock-add-keywords 'js-mode '(("\\bconsole\\.[A-z]+\\b" 0 'font-lock-constant-face)))
(font-lock-add-keywords 'coffee-mode '(("\\bconsole\\.[A-z]+\\b" 0 'font-lock-constant-face)))

;; Highlight ES6 Arrow function
(font-lock-add-keywords 'js-mode '(("=>" 0 'font-lock-function-name-face)))


;; ======================================================
;; For Angular JS anti-human function [2017-06-06 火 14:17]
;; ======================================================
(defun angular-js-function-injection-fill-strings ()
  (interactive)
  (when (and (or (eq major-mode 'js-mode)
                 (eq major-mode 'js2-mode)
                 (eq major-mode 'yajade-mode)
                 (eq major-mode 'jade-mode)))
    (let* ((beg (progn (move-beginning-of-line 1) (point) ))
           (end (progn (move-end-of-line 1) (point) ))
           (current-line (buffer-substring-no-properties beg end))
           (matched (string-match "\\(.+\\)\\[\\(['\", \\$A-z0-9]+\\)\\(?:function *(\\(.+\\))\\|(\\(.+\\)) *=>\\)" current-line)))
      (when matched
        (let* ((old-str (match-string-no-properties 0 current-line))
               (head (match-string-no-properties 1 current-line))
               (raw-args (or (match-string-no-properties 3 current-line) (match-string-no-properties 4 current-line)))
               (args-list (split-string (replace-regexp-in-string " " "" raw-args t) ","))
               (formatted-args (string-join args-list ", "))
               (formatted-string-args (mapconcat (lambda (x) (format "\"%s\"" x)) args-list ", "))
               (new-str (concat head "[" formatted-string-args ", (" formatted-args ") =>")))
          (delete-region beg (+ beg (length old-str)))
          (goto-char beg)
          (insert new-str)
          )))
    ))

(define-key js-mode-map (kbd "C-c C-a") 'angular-js-function-injection-fill-strings)
;; (font-lock-add-keywords 'js-mode '(("(\\([$A-z0-9_]+\\)\\(?:[ \n]*,[ \n]*\\([A-z0-9$_]+\\)\\)*[\n ]*) *=>" 0 'font-lock-variable-face)))
;; ======================================================
;; Comint
;; ======================================================
(require 'js-comint)
(cond ((eq system-type 'darwin)
       (setq inferior-js-program-command "node"))
      ((eq system-type 'gnu/linux)
       (setq inferior-js-program-command "node")))

(setq process-coding-system-alist
      (cons '("js" utf-8 . utf-8) process-coding-system-alist)) ;shit didn't work

(defun js-run-with-shell-command ()
  (interactive)
  (save-buffer)
  (shell-command (format "node %s" (buffer-real-name))))
(define-key js2-mode-map (kbd "<f5>") 'js-run-with-shell-command)


(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        (rainbow-delimiters-mode)
        ;; Deal with some prompt nonsense
        (add-to-list 'comint-preoutput-filter-functions
                     (lambda (output)
                       (replace-regexp-in-string ".*1G\.\.\..*5G" "..."
                                                 (replace-regexp-in-string ".*1G.*3G" "&gt;" output))))))
;; ======================================================
;; Shell
;; ======================================================
;; open javascript interactive shell.
(defun jsc ()
  (interactive)
  (eshell "JSC")
  (insert "rhino")
  (eshell-send-input ""))


(provide 'rc-js)
;;; rc-javascript.el ends here
