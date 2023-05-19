;;; rc-magit.el ---                                  -*- lexical-binding: t; -*-

;;======================================================
;; Magit!
;;======================================================
(with-eval-after-load 'magit
  (require 'magit)
  (setq git-commit-summary-max-length 600)
  (remove-hook 'git-commit-setup-hook 'git-commit-turn-on-auto-fill)

  )
(with-eval-after-load 'magit-log
  ;; (defun --my-magit-log-hack ()
  ;;   (put 'magit-log-mode 'magit-log-default-arguments
  ;; 	 '("--graph" "-n256" "--decorate" "--color" "--decorate-refs-exclude=*_[0-9][0-9][0-9][0-9][0-9][0-9]"))
  ;;   (put 'magit-log-select-mode 'magit-log-default-arguments
  ;; 	 '("--graph" "-n256" "--decorate" "--color" "--decorate-refs-exclude=*_[0-9][0-9][0-9][0-9][0-9][0-9]"))
  ;;   )
  ;; (add-hook 'magit-log-mode-hook '--my-magit-log-hack)

  ;; (transient-define-argument my-magit:--exclude-refs ()
  ;;   :description "Exclude refs / tags"
  ;;   :class 'transient-option
  ;;   :key "=e"
  ;;   :argument "--exclude-ref="
  ;;   :reader #'my-magit-transient-read-glob-pattern)

  ;; (defun my-magit-transient-read-glob-pattern (prompt initial-input history)
  ;;   (magit-completing-read
  ;;    prompt
  ;;    (mapcar (lambda (line)
  ;;              (save-excursion
  ;; 		 (and (string-match "\\`[\s\t]+[0-9]+\t" line)
  ;;                     (list (substring line (match-end 0))))))
  ;;            (magit-git-lines "shortlog" "-n" "-s" "-e" "HEAD"))
  ;;    nil nil initial-input history))

  ;; (transient-append-suffix 'magit-log "T"
  ;;   (my-magit:--exclude-ref))
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


;; =======================================
;; Enhanced git cherry-pick
;; =======================================

;; https://stackoverflow.com/questions/23299314/finding-the-exit-code-of-a-shell-command-in-elisp
(defun my-magit-run-process (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a dotted tuple."
  (with-temp-buffer
    (cons (apply 'call-process program nil (current-buffer) nil args)
          (buffer-string))))

;; See magit-cherry-pick
;; (magit-cherry-copy hash '("--ff"))
(defun my-magit-cherry-pick-current-commit ()
  (interactive)
  (let* ((hash-head (car (string-split (shell-command-to-string "git rev-parse HEAD"))))
	 (hash-cherrypicked (my-magit-log-get-hash-of-current-line))
	 (cherrypick-result (car (my-magit-run-process "git" "cherry-pick" "--ff" hash-cherrypicked))))
    (if (not (eq cherrypick-result 0))
	(progn
	  (message "Cherry-pick failed. Please check manually.")
	  )
      (let* ((ori-msg (shell-command-to-string "git log -1 --pretty=%B"))
	     (fmtted-branches (mapconcat (lambda(x) (format "\`%s\`" x))
					 (my-magit-get-branches-containing-commit hash-cherrypicked) ", "))
	     (new-msg (format "%s
--------------
(This commit is cherry-picked from %s , which is contained in the following branch(s): %s)" ori-msg hash-cherrypicked fmtted-branches)))
	(shell-command (format "git commit --amend -m \"%s\"" (my-shell-arg-escape new-msg)))
	(message (shell-command-to-string "git log -1"))
	))
    ))

;; (defun aaa ()
;; (interactive)
;; (message "%S" (my-magit-get-branches-containing-commit (my-magit-log-get-hash-of-current-line))))


(defun my-magit-get-branches-containing-commit (hash)
  (let* ((result (my-magit-run-process "git" "branch" "--all" "--contains" hash))
	 (lines (butlast (split-string (cdr result) "\n")))
	 (branches-raw (mapcar (lambda (x) (substring x 2)) lines))
	 (branches-filtered (cl-delete-duplicates
			     (mapcar (lambda (x)
				       (replace-regexp-in-string "^remotes/[^/]+/" "" x))
				     branches-raw)
			     :test #'equal)))

    branches-filtered))



(defun my-shell-arg-escape (string)
  "Replace html entities.
Example:
(html-enetities-convert \"&gt\;\")
=> \">\""
  (mapc (lambda (x)
	  (if (string-match (car x) string)
	      (setq string (string-replace (car x) (cdr x) string))
	    ))
        '(
	  ("\"" . "\\\"")
	  ("\`" . "\\\`")
	  )
	)
  string)

(defun my-magit-log-get-hash-of-current-line ()
  (let* ((section (magit-current-section))
	 (hash-short (oref section value))
	 (hash-full (magit-rev-parse hash-short)))
    hash-full))

(with-eval-after-load 'magit
  )

(provide 'rc-magit)
;;; rc-magit.el ends here
