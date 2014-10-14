;;; rc-c.el ---                                      -*- lexical-binding: t; -*-

;;======================================================
;; C
;;======================================================
(add-hook 'c-mode-hook
 	  (lambda ()
	    (require 'auto-complete-c-headers)
	    (add-to-list 'ac-sources 'ac-source-c-headers)
	    (require 'auto-complete-clang)

	    (c-set-style "linux")
	    (defun c-compile-current-file ()
	      (interactive)
	      (save-buffer)
	      (shell-command (format "gcc -Wall %s -o %s"
				     (buffer-real-name)
				     (file-name-base))))
	    (define-key c-mode-map (kbd "<f5>") 'c-compile-current-file)
 	    ;(add-to-list 'ac-sources 'ac-source-c-headers)
 	    ;(add-to-list 'ac-sources 'ac-source-c-header-symbols t)
	    ))



(provide 'rc-c)
;;; rc-c.el ends here
