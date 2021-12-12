;;; rc-ruby.el ---                                   -*- lexical-binding: t; -*-

;;======================================================
;; Ruby
;;======================================================


(add-hook 'ruby-mode-hook 'my-ruby-hook)

(defun my-ruby-hook ()
  ;; Smartparens can be used to match "end"s in Ruby code
  (require 'smartparens)
  (require 'smartparens-ruby)
  (show-smartparens-mode)
  (enh-ruby-mode)
  (require 'inf-ruby)
  (defun ruby-run-current-file ()
    (interactive)
    (save-buffer)
    (shell-command (format "chmod +x %s" (buffer-real-name)))
    (shell-command (format "ruby %s" (buffer-real-name))))
  (require 'ruby-mode)
  (setq ruby-indent-level 4)
  (setq enh-ruby-indent-level 4)

  (define-key ruby-mode-map (kbd "<f5>") 'ruby-run-current-file)
  (define-key enh-ruby-mode-map (kbd "<f5>") 'ruby-run-current-file)
  )


(provide 'rc-ruby)
;;; rc-ruby.el ends here
