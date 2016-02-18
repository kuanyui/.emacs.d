;;; rc-c.el ---                                      -*- lexical-binding: t; -*-

;;======================================================
;; C
;;======================================================
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)

(add-hook 'c-mode-hook
 	  (lambda ()
	    (c-set-style "linux")
	    (defun c-compile-current-file ()
	      (interactive)
	      (save-buffer)
	      (if (file-exists-p (file-name-base)) (delete-file (file-name-base)))
	      (shell-command (format "gcc -Wall %s -o %s"
				     (buffer-real-name)
				     (file-name-base))))
	    (define-key c-mode-map (kbd "<f5>") 'c-compile-current-file)
					;(add-to-list 'ac-sources 'ac-source-c-headers)
					;(add-to-list 'ac-sources 'ac-source-c-header-symbols t)
	    (define-key c-mode-map (kbd "C-c M-n") 'highlight-symbol-at-point)
	    (define-key c-mode-map (kbd "M-n")'highlight-symbol-next)
	    (define-key c-mode-map (kbd "M-p")'highlight-symbol-prev)
	    (define-key c-mode-map (kbd "C-c M-p") 'highlight-symbol-query-replace)
	    ))



(provide 'rc-c)
;;; rc-c.el ends here
