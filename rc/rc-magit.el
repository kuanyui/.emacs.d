;;; rc-magit.el ---                                  -*- lexical-binding: t; -*-

;;======================================================
;; Magit!
;;======================================================

(require 'magit)
(global-set-key (kbd "C-x g s") 'magit-status)
(global-set-key (kbd "C-x g l") 'magit-log)
(define-key magit-mode-map (kbd "C-c d") 'magit-diff-staged)


(defun magit-log-all ()
  (interactive)
  (magit-key-mode-popup-logging)
  (magit-key-mode-toggle-option (quote logging) "--all"))
(define-key magit-mode-map (kbd "l") 'magit-log-all)

(require 'git-timemachine)


(provide 'rc-magit)
;;; rc-magit.el ends here
