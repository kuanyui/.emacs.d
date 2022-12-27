;;; rc-magit.el ---                                  -*- lexical-binding: t; -*-

;;======================================================
;; Magit!
;;======================================================
(with-eval-after-load 'magit
  (require 'magit)
  (setq git-commit-summary-max-length 600)
  (remove-hook 'git-commit-setup-hook 'git-commit-turn-on-auto-fill)
  )
(global-set-key (kbd "C-x g s") 'magit-status)
(global-set-key (kbd "C-x g l") 'magit-log)
(global-set-key (kbd "C-x g b") 'magit-blame)

(add-to-list 'auto-mode-alist '(".gitmodules" . conf-mode))

;; (global-git-gutter-mode +1)
;; (setq magit-diff-refine-hunk 'all)
;; (setq magit-last-seen-setup-instructions "1.4.0")
;; (setq git-commit-summary-max-length 500)
;; (add-to-list 'load-path "~/.emacs.d/lisps/magit/lisp/")
;; (require 'magit)
;;
;; (defun my-magit-status ()
;; (interactive)
;; (my-whitespace-cleanup)
;; (magit-status))
;;
;; (setq magit-auto-revert-mode t)

;; (defun my-magit-insert-pull-request (interactive) (insert "fetch = +refs/pull/*/head:refs/remotes/origin/pr/*"))
;;
;; ;; (defun magit-log-all ()
;; ;;   (interactive)
;; ;;   (magit-log-popup)
;; ;;   (magit-key-mode-toggle-option (quote logging) "--all"))
;; ;; (define-key magit-mode-map (kbd "l") 'magit-log-all)
;;
;; (require 'git-timemachine)
;;
;;
;; (getenv "GIT_SSH_COMMAND")
;;
;; (defun sshkey-selector ()
;; (interactive)
;; (let* ((keys (mapcar (lambda (x) (replace-regexp-in-string "[.]pub$" "" x))
;; (directory-files "~/.ssh/" nil "[.]pub$")))
;; (chosen (ido-completing-read "Select a key: " keys)))
;; (setenv "GIT_SSH_COMMAND" (format "ssh -o 'IdentitiesOnly=yes' -i ~/.ssh/%s" chosen))))


;; Copied From https://github.com/magit/magit/discussions/4826#discussioncomment-4389196

(defun my/magit-refs-toggle-tags ()
  "Toggle showing tags in `magit-refs-mode'.
This only affects the current buffer and is useful if you do not
show tags by default."
  (interactive)
  (let ((pos (point)))
    (if (memq 'magit-insert-tags magit-refs-sections-hook)
        (kill-local-variable 'magit-refs-sections-hook)
      (setq-local magit-refs-sections-hook
                  (append magit-refs-sections-hook
                          '(magit-insert-tags))))
    (magit-refresh-buffer)
    (goto-char pos)))

(with-eval-after-load 'magit-refs
  (remove-hook 'magit-refs-sections-hook #'magit-insert-tags)
  (define-key magit-refs-mode-map (kbd "C-c C-t") #'my/magit-refs-toggle-tags))

(provide 'rc-magit)
;;; rc-magit.el ends here
