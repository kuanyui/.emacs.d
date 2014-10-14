;;; rc-python.el ---                                 -*- lexical-binding: t; -*-
;;======================================================
;; Python
;;======================================================

(setq org-babel-python-command "python3")

(setq
 python-shell-interpreter "python3"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(define-key python-mode-map (kbd "<f6>") 'python2-compile-with-shell-command)
(defun python2-compile-with-shell-command ()
  (interactive)
  (save-buffer)(shell-command (format "python2 %s" (buffer-real-name))))
(define-key python-mode-map (kbd "<f5>") 'python3-compile-with-shell-command)
(defun python3-compile-with-shell-command ()
  (interactive)
  (save-buffer)(shell-command (format "python3 %s" (buffer-real-name))))

;; Enable rainbow-delimiters-mode in Python mode
(add-hook 'python-mode-hook (lambda () (rainbow-delimiters-mode-enable)))

;; Info-look
(require 'info-look)
(info-lookup-add-help
 :mode 'python-mode
 :regexp "[[:alnum:]_]+"
 :doc-spec
 '(("(python)Index" nil "")))
(require 'python)
(require 'python-info)

;; M-RET 自動註解換行（評估看看是否M-q就夠用了）
(define-key python-mode-map (kbd "M-RET")
  (lambda () (interactive) (newline) (comment-dwim nil)))

;;======================================================
;; Jedi: Auto-complete for Python
;;======================================================

(require 'jedi)
;; (setq jedi:environment-root "jedi")  ; or any other name you like
;; (setq jedi:environment-virtualenv
;;       (append python-environment-virtualenv
;;               '("--python" "/usr/bin/python3")))
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)


(provide 'rc-python)
;;; rc-python.el ends here
