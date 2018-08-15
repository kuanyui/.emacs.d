;;; rc-makefile.el ---                               -*- lexical-binding: t; -*-

(defun my-makefile-update-phony ()
  (interactive)
  (save-excursion
    (save-match-data
      (let (targets)
        (goto-char (point-min))
        (while (re-search-forward "^\\([^\n\t ]+?\\):" nil :no-error)
          (let ((target (match-string-no-properties 1)))
            (if (not (equal ".PHONY" target))
                (push target targets))))
        (goto-char (point-min))
        (while (re-search-forward "^[.]PHONY:.+\n" nil :no-error)
          (delete-region (match-beginning 0) (match-end 0)))
        (goto-char (point-min))
        (insert (format ".PHONY: %s\n" (string-join (reverse targets) " ")))
        ))
    ))


(provide 'rc-makefile)
;;; rc-makefile.el ends here
