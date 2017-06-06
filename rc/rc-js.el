;;; rc-javascript.el ---                             -*- lexical-binding: t; -*-

;; open javascript interactive shell.
(defun jsc ()
  (interactive)
  (eshell "JSC")
  (insert "rhino")
  (eshell-send-input ""))


;;Javascript
;; js2
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(setq js2-strict-missing-semi-warning nil)

(require 'js-comint)
(cond ((eq system-type 'darwin)
       (setq inferior-js-program-command "node"))
      ((eq system-type 'gnu/linux)
       (setq inferior-js-program-command "~/.emacs.d/node-v6.5.0-linux-x64/bin/node"))
      )

(setq process-coding-system-alist
      (cons '("js" utf-8 . utf-8) process-coding-system-alist)) ;shit didn't work


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

(add-hook 'js2-mode-hook 'js-comint-my-conf)
(add-hook 'js2-mode-hook
          (lambda () (push '("function" . ?ƒ) prettify-symbols-alist)))
(add-hook 'js2-mode-hook #'rainbow-delimiters-mode)
(add-hook 'js-mode-hook #'rainbow-delimiters-mode)
(setq js2-basic-offset 2)

(defun js-comint-my-conf ()
  (local-set-key "\C-x\C-e" 'js-send-last-sexp)
  (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
  (local-set-key "\C-cb" 'js-send-buffer)
  (local-set-key (kbd "<f5>") 'js-send-buffer)
  (local-set-key "\C-c\C-l" 'js-send-buffer-and-go)
  (local-set-key "\C-cl" 'js-load-file-and-go)
  )

(font-lock-add-keywords 'coffee-mode '(("\\$scope" 0 'font-lock-builtin-face)))
(font-lock-add-keywords 'js-mode '(("\\$scope" 0 'font-lock-builtin-face)))
(font-lock-add-keywords 'js2-mode '(("\\$scope" 0 'font-lock-builtin-face)))
(font-lock-add-keywords 'js-mode '(("\\b[0-9A-z]+?:" 0 'font-lock-type-face)))

(font-lock-add-keywords 'js-mode '(("\\bconsole\\.[A-z]+\\b" 0 'font-lock-constant-face)))
(font-lock-add-keywords 'coffee-mode '(("\\bconsole\\.[A-z]+\\b" 0 'font-lock-constant-face)))

(font-lock-add-keywords 'js-mode '(("=>" 0 'font-lock-function-name-face)))

;; ======================================================
;; For Angular JS anti-human function [2017-06-06 火 14:17]
;; ======================================================
(defun angular-js-function-injection-sync ()
  (interactive)
  (when (and (or (eq major-mode 'js-mode)
                 (eq major-mode 'js2-mode)
                 (eq major-mode 'pug-mode)
                 (eq major-mode 'jade-mode))
             (memq this-command '(self-insert-command delete-backward-char kill-word delete-char delete-forward-char)))
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
               (new-str (concat  head "[" formatted-string-args ", (" formatted-args ") =>")))
          (delete-region beg end)
          (insert new-str)
          (goto-char (- (length new-str) (length old-str)))
          )))
    ))
(add-hook 'post-command-hook 'angular-js-function-injection-sync)


;; (font-lock-add-keywords 'js-mode '(("(\\([$A-z0-9_]+\\)\\(?:[ \n]*,[ \n]*\\([A-z0-9$_]+\\)\\)*[\n ]*) *=>" 0 'font-lock-variable-face)))



;;(autoload 'tern-mode "tern.el" nil t)
;;(add-hook 'js-mode-hook (lambda () (tern-mode t)))

;; (define-key js2-mode-map (kbd "<f5>") 'call-nodejs-command)
(defun call-nodunejs-command ()
  (interactive)
  (save-buffer)(shell-command (format "node %s" (buffer-real-name))))

(defun js-buffer-to-multiline-string ()
  (interactive)
  (kill-new
   (mapconcat
    (lambda (line)
      (concat "'" line "'"))
    (remove-if (lambda (str) (eq (length str) 0))
               (split-string (buffer-string) "\n"))
    " +\n")
   )
  (message "Copied!")
  )

(provide 'rc-js)
;;; rc-javascript.el ends here
