;;; rc-eww.el ---                                    -*- lexical-binding: t; -*-

(defun eww-my-guess-open ()
  (interactive)
  (let* ((guess (ffap-prompter))
         (url? (ffap-url-p guess)))
    (if url?
        (eww-browse-url guess)
      (message "This shit seem not a valid URL"))))



(provide 'rc-eww)
;;; rc-eww.el ends here
