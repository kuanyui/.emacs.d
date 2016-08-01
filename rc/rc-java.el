;;; rc-java.el ---                                   -*- lexical-binding: t; -*-

(add-hook 'java-mode-hook 'java-config)

(defun java-config ()
  (defun java-compile-and-execute-current-file ()
    (interactive)
    (save-buffer)
    (if (file-exists-p (file-name-base)) (delete-file (file-name-base)))
    (shell-command (format "javac %s; java %s"
			   (buffer-real-name)
			   (file-name-base))))
  (define-key java-mode-map (kbd "<f5>") 'java-compile-and-execute-current-file)
  
  (add-hook 'java-mode-hook 'highlight-symbol-mode)
  ;; Rainbow delimiters
  (rainbow-delimiters-mode t)

  )
(provide 'rc-java)
;;; rc-java.el ends here
