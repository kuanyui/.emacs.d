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
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(require 'js-comint)
(setq inferior-js-program-command "js")

(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        ;; Deal with some prompt nonsense
        (add-to-list 'comint-preoutput-filter-functions
                     (lambda (output)
                       (replace-regexp-in-string ".*1G\.\.\..*5G" "..."
                     (replace-regexp-in-string ".*1G.*3G" "&gt;" output))))))

(add-hook 'js2-mode-hook 'js-comint-my-conf)
(add-hook 'js2-mode-hook
          (lambda () (push '("function" . ?ƒ) prettify-symbols-alist)))
(add-hook 'js2-mode-hook #'rainbow-delimiters-mode)


(defun js-comint-my-conf ()
  (local-set-key "\C-x\C-e" 'js-send-last-sexp)
  (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
  (local-set-key "\C-cb" 'js-send-buffer)
  (local-set-key (kbd "<f5>") 'js-send-buffer)
  (local-set-key "\C-c\C-l" 'js-send-buffer-and-go)
  (local-set-key "\C-cl" 'js-load-file-and-go)

  )

(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

;; (define-key js2-mode-map (kbd "<f5>") 'call-nodejs-command)
(defun call-nodejs-command ()
  (interactive)
  (save-buffer)(shell-command (format "node %s" (buffer-real-name))))


(provide 'rc-javascript)
;;; rc-javascript.el ends here
