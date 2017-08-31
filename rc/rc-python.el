;;; rc-python.el ---                                 -*- lexical-binding: t; -*-
;;======================================================
;; Python
;;======================================================
(require 'python)
(setq org-babel-python-command "python3")
(require 'flycheck)
;; flycheck-python-flake8-executable
;; flycheck-python-pycompile-executable
;; flycheck-python-pylint-executable

(add-hook 'python-mode-hook 'flycheck-mode)
(setq flycheck-pylintrc "~/.pylintrc")
(setq flycheck-pylint-use-symbolic-id nil)  ;; Fuck useless

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

(define-key python-mode-map (kbd "C-c C-r") 'revert-buffer-without-confirm)
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
(require 'python-info)

;; M-RET 自動註解換行（評估看看是否M-q就夠用了）
(define-key python-mode-map (kbd "M-RET")
  (lambda () (interactive) (newline) (comment-dwim nil)))

;; ======================================================
;; Venv
;; ======================================================
;; Fuck the Emacs. Useless at all.
(require 'virtualenvwrapper)
(add-hook 'python-mode-hook 'projectile-mode)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(setq projectile-switch-project-action 'venv-projectile-auto-workon)

(setq venv-dirlookup-names '(".venv" "venv"))
;;(setq-default mode-line-format (cons '(:exec venv-current-name) mode-line-format))

;; ======================================================
;; elpy
;; ======================================================

(elpy-enable)
(setq elpy-modules '(elpy-module-sane-defaults
                     elpy-module-company
                     elpy-module-eldoc
                     elpy-module-flymake
                     elpy-module-pyvenv
                     elpy-module-yasnippet
                     elpy-module-django))
;;======================================================
;; Company-Jedi : Completion for Python
;;======================================================
;; NOTICE: Company-jedi is not related to jedi-mode (Auto-Completion-based)!

;; (require 'company-jedi)
;; (autoload 'jedi:setup "jedi" nil t)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (defun my/python-mode-hook ()
;; (add-to-list 'company-backends 'company-jedi)
;; (define-key python-mode-map (kbd "<f1>") #'jedi:show-doc)
;; )
;; (add-hook 'python-mode-hook 'my/python-mode-hook)

;; jedi:environment-root
;; python-environment-directory
;; python-environment-default-root-name

;; ======================================================
;; Anaconda (Completion)
;; ======================================================
;;(add-hook 'python-mode-hook 'anaconda-mode)
;;(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
;;(eval-after-load "company"
;;'(add-to-list 'company-backends 'company-anaconda))
;;(eval-after-load "company"
;;'(add-to-list 'company-backends '(company-anaconda :with company-capf)))
;; ======================================================
;; Elpy
;; ======================================================

;;(elpy-enable)

(provide 'rc-python)
;;; rc-python.el ends here
