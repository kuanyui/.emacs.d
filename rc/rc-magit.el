;;; rc-magit.el ---                                  -*- lexical-binding: t; -*-

;;======================================================
;; Magit!
;;======================================================

(setq magit-diff-refine-hunk 'all)
(setq magit-last-seen-setup-instructions "1.4.0")
(add-to-list 'load-path "~/.emacs.d/lisps/magit/lisp/")
(require 'magit)

(defun my-magit-status ()
  (interactive)
  (my-whitespace-cleanup)
  (magit-status))

(setq magit-auto-revert-mode t)
(global-set-key (kbd "C-x g s") 'my-magit-status)
(global-set-key (kbd "C-x g l") 'magit-log)
(global-set-key (kbd "C-x g b") 'magit-blame)

(defun my-magit-insert-pull-request (interactive) (insert "fetch = +refs/pull/*/head:refs/remotes/origin/pr/*"))

;; (defun magit-log-all ()
;;   (interactive)
;;   (magit-log-popup)
;;   (magit-key-mode-toggle-option (quote logging) "--all"))
;; (define-key magit-mode-map (kbd "l") 'magit-log-all)

(require 'git-timemachine)


(getenv "GIT_SSH_COMMAND")

(defun sshkey-selector ()
  (interactive)
  (let* ((keys (mapcar (lambda (x) (replace-regexp-in-string "[.]pub$" "" x))
                       (directory-files "~/.ssh/" nil "[.]pub$")))
         (chosen (ido-completing-read "Select a key: " keys)))
    (setenv "GIT_SSH_COMMAND" (format "ssh -o 'IdentitiesOnly=yes' -i ~/.ssh/%s" chosen))))

(provide 'rc-magit)
;;; rc-magit.el ends here
