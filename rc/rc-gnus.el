;;; rc-gnus.el ---                                   -*- lexical-binding: t; -*-

;;======================================================
;; gnus
;;======================================================
;;

(setq gnus-select-method
      '(nnimap "gmail"
           (nnimap-address "imap.gmail.com")
           (nnimap-server-port 993)
           (nnimap-stream ssl)))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 user-mail-address nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq gnus-default-charset 'utf-8)

;; 發信用coding-system
(setq mm-coding-system-priorities '(utf-8))

(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-score)))

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-number
        gnus-thread-sort-by-date))

;; 避免使用者在神智不清的情況下幹出無法挽回的蠢事
(setq message-confirm-send t)
(defun message-test-mind-before-send ()
  (interactive)
  (let* ((a (random 50))
         (b (random 15))
         (ans (+ a b))
         (input (string-to-int (read-from-minibuffer (format "%s + %s = " a b)))))
    (if (eq input ans)
        (message-send-and-exit)
      (progn (message "Wrong answer, please try again")
             (sleep-for 2)
             (message-test-mind-before-send)
             ))))
(add-hook 'message-mode-hook
	  (lambda ()
	    (define-key message-mode-map (kbd "C-c C-c") 'message-test-mind-before-send)
	    ))


(provide 'rc-gnus)
;;; rc-gnus.el ends here
