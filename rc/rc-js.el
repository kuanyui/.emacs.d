;;; rc-javascript.el ---                             -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(setq js2-include-node-externs t)  ;; process, global... etc.

(defun my-js2-fold-functions ()
  (interactive)
  (js2-mode-hide-functions))

(defun my-js2-fold-all ()
  (interactive)
  (js2-mode-hide-functions)
  (js2-mode-hide-comments))

(defun my-js2-expand ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((bounds (js2-mode-invisible-overlay-bounds)))
      (while (and (not (eolp))
                  (null bounds))
        (right-char)
        (setq bounds (js2-mode-invisible-overlay-bounds)))
      (if bounds
          (js2-mode-show-element)
        (message "Not in a folded line"))
      )))

(defun my-js2-expand-all ()
  (interactive)
  (js2-mode-show-functions)
  (js2-mode-show-comments))

;; ======================================================
;; My JavaScript config
;; ======================================================
;; (add-to-list 'load-path "~/.emacs.d/git/flow-js2-mode")
;; (require 'flow-js2-mode)

(add-hook 'js2-mode-hook 'my-js2-conf)
(defun my-js2-conf ()
  (my--js2-flow-autoconf)
  (my--js2-comint-conf)
  (setq js2-basic-offset 2)
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-indent-switch-body t)
  (define-key js2-mode-map (kbd "C-c f f") 'my-js2-fold-functions)
  (define-key js2-mode-map (kbd "C-c f F") 'my-js2-fold-all)
  (define-key js2-mode-map (kbd "C-c f e") 'my-js2-expand)
  (define-key js2-mode-map (kbd "C-c f E") 'my-js2-expand-all)
  (define-key js2-mode-map (kbd "<f5>") 'js-run-with-shell-command)
  (rainbow-delimiters-mode)
  (flycheck-mode 1)
  (company-mode-on)
  )

(defun my--js2-comint-conf ()
  (local-set-key "\C-x\C-e" 'js-send-last-sexp)
  (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
  (local-set-key "\C-cb" 'js-send-buffer)
  (local-set-key (kbd "<f5>") 'js-run-with-shell-command)
  (local-set-key (kbd "C-x <f5>") 'js-send-buffer)
  (local-set-key "\C-c\C-l" 'js-send-buffer-and-go)
  (local-set-key "\C-cl" 'js-load-file-and-go)
  )
(require 'flow-minor-mode)
(defun my--js2-flow-autoconf ()
  (when (my-js-flow-tag-exists-p)
    ;; (flow-js2-mode) ; this still has lots of errors.
    (js-mode)  ;; fallback to js-mode
    ;; (flow-minor-mode)
    ;; (lsp-js-flow-enable)
    (require 'eglot)
    (eglot-ensure)
    (message "Flow detected, fallback to js-mode + flow-minor-mode"))
  )

(defun my-js-flow-tag-exists-p ()
  "Return true if the '// @flow' tag is present in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (stop found)
      (while (not stop)
        (when (not (re-search-forward "[^\n[:space:]]" nil t))
          (setq stop t))
        (if (equal (point) (point-min))
            (setq stop t)
          (backward-char))
        (cond ((or (looking-at "//+[ ]*@flow")
                   (looking-at "/\\**[ ]*@flow"))
               (setq found t)
               (setq stop t))
              ((looking-at "//")
               (forward-line))
              ((looking-at "/\\*")
               (when (not (re-search-forward "*/" nil t))
                 (setq stop t)))
              (t (setq stop t))))
      found)))

;; (require 'lsp-mode)
;; (require 'lsp-javascript-flow)
;; (lsp-define-stdio-client
;; lsp-js-flow "js"
;; (lambda () (locate-dominating-file "." "package.json"))
;; nil
;; :ignore-messages '("\[INFO].*?nuclide")
;; :command-fn (lambda () `("flow-language-server" "--stdio")))
;;

;; ======================================================
;; flow
;; ======================================================
;; (require 'flycheck-flow)
;; (with-eval-after-load 'flycheck
;;   (flycheck-add-mode 'javascript-flow 'flow-minor-mode)
;;   (flycheck-add-mode 'javascript-eslint 'flow-minor-mode)
;;   (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))

;; (with-eval-after-load 'company
;; (add-to-list 'company-backends 'company-flow))

;; ======================================================
;; mmm-mode
;; ======================================================
(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
(setq mmm-parse-when-idle 't)

;; Syntax-highlight For coffee-template
(font-lock-add-keywords
 'coffee-mode
 '(("['\"][0-9A-Za-z-_]+?[\"']:" 0 'font-lock-type-face prepend) ;  'user-name':"
   ("['\"][.#].*?\\([.][.0-9A-Za-z-_]+\\)[\"']" 1 'font-lock-variable-name-face prepend) ; '.class-name'
   ("['\"]\\([.][0-9A-Za-z-_]+\\)" 1 'font-lock-variable-name-face prepend) ; '#class-name' in the beginning
   ("['\"]\\([#][0-9A-Za-z-_]+\\)" 1 'font-lock-keyword-face prepend) ; '#id' in the beginning
   )
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


;; ======================================================
;; Flycheck
;; ======================================================

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; ======================================================
;; TypeScript
;; ======================================================

(defun my/use-tslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (tslint (and root
                      (expand-file-name "node_modules/.bin/tslint"
                                        root))))
    (when (and tslint (file-executable-p tslint))
      (setq-local flycheck-typescript-tslint-executable tslint))))
(add-hook 'flycheck-mode-hook #'my/use-tslint-from-node-modules)

(add-hook 'typescript-mode-hook 'my-ts-conf)
(defun my-ts-conf ()
  (rainbow-delimiters-mode)
  (flycheck-mode 1)
  (company-mode-on)
  (require 'eglot)
  (eglot-ensure)
  )


(provide 'rc-js)
;;; rc-javascript.el ends here
