;;; rc-magit.el ---                                  -*- lexical-binding: t; -*-

;;======================================================
;; Magit!
;;======================================================

(setq magit-last-seen-setup-instructions "1.4.0")
(add-to-list 'load-path "~/.emacs.d/lisps/magit/lisp/")
(require 'magit)

(setq magit-auto-revert-mode t)
(global-set-key (kbd "C-x g s") 'magit-status)
(global-set-key (kbd "C-x g l") 'magit-log)
(global-set-key (kbd "C-x g b") 'magit-blame)
(define-key magit-mode-map (kbd "C-c d") 'magit-diff-staged)


;; (defun magit-log-all ()
;;   (interactive)
;;   (magit-log-popup)
;;   (magit-key-mode-toggle-option (quote logging) "--all"))
;; (define-key magit-mode-map (kbd "l") 'magit-log-all)

(require 'git-timemachine)


(provide 'rc-magit)
;;; rc-magit.el ends here
