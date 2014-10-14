;;; rc-magit.el ---                                  -*- lexical-binding: t; -*-

;;======================================================
;; Magit!
;;======================================================

(require 'magit)
(global-set-key (kbd "C-x g s") 'magit-status)
(global-set-key (kbd "C-x g l") 'magit-log)
(define-key magit-mode-map (kbd "C-c d") 'magit-diff-staged)



(provide 'rc-magit)
;;; rc-magit.el ends here
