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
(with-eval-after-load 'magit-log
  (define-key magit-log-mode-map (kbd "M-A") #'my-magit-cherry-pick-current-commit)
  )

(require 'f)
;; https://stackoverflow.com/questions/23299314/finding-the-exit-code-of-a-shell-command-in-elisp
(defun my-magit-run-process (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a dotted tuple."
  (with-temp-buffer
    (cons (apply 'call-process program nil (current-buffer) nil args)
          (buffer-string))))

(defun my-magit-shell-command-to-string (cmd)
  "Remove the newline in the end of file."
  (string-join (butlast (split-string (shell-command-to-string cmd) "\n")) "\n"))

;; See magit-cherry-pick
;; (magit-cherry-copy hash '("--ff"))
(defun my-magit-cherry-pick-current-commit ()
  (interactive)
  (let* ((hash-head (my-magit-shell-command-to-string "git rev-parse HEAD"))
	 (hash-cherrypicked (my-magit-log-get-hash-of-current-line))
	 (branch-head (format "`%s`" (my-magit-shell-command-to-string "git branch --show")))
	 (branches-cherrypicked (mapconcat (lambda(x) (format "\`%s\`" x))
					   (my-magit-get-branches-containing-commit hash-cherrypicked) ", "))
	 (cherrypick-result (car (my-magit-run-process "git" "cherry-pick" "--ff" hash-cherrypicked)))
	 (NEW_MSG_TEMPLATE "%s
--------------
(This commit is created via cherry-pick by HEAD branch %s. The cherry-picked source commit is %s , which is contained in the following branch(s): %s"))
    (if (not (eq cherrypick-result 0))
	(let* ((ori-msg-fpath (f-join (my-magit-shell-command-to-string "git rev-parse --show-toplevel") ".git/MERGE_MSG"))
	       (ori-msg (f-read-text ori-msg-fpath 'utf-8))
	       (new-msg (format NEW_MSG_TEMPLATE ori-msg branch-head hash-cherrypicked branches-cherrypicked)))
	  (f-write-text new-msg 'utf-8 ori-msg-fpath)
	  (message (propertize (format "
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
[ATTENTION] Cherry-pick failed. Please check manually.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
\n\n%s" new-msg) :foreground "#ff6666"))
	  )
      (let* ((ori-msg (my-magit-shell-command-to-string "git log -1 --pretty=%B"))
	     (new-msg (format NEW_MSG_TEMPLATE ori-msg branch-head hash-cherrypicked branches-cherrypicked)))
	(shell-command (format "git commit --amend -m \"%s\"" (my-shell-arg-escape new-msg)))
	(message (my-magit-shell-command-to-string "git log -1"))
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

;; Forked from magit-log.el
;; Highlight [.*?], \(.*?\) in commit message.
(require 's)
(defvar magit-log-format-message-function #'my-magit-log-propertize-keywords)
(defun my-magit-log-propertize-keywords (_rev msg)
  (let ((boundary 0))
    (when (string-match "^\\(?:squash\\|fixup\\)! " msg boundary)
      (setq boundary (match-end 0))
      (magit--put-face (match-beginning 0) (1- boundary)
		       'magit-keyword-squash msg))
    (when magit-log-highlight-keywords
      (mapc (lambda (x) (magit--put-face (car x) (cdr x) 'magit-keyword msg))
	    (s-matched-positions-all "\\[[^ ]+\\]" msg 0))
      (mapc (lambda (x) (magit--put-face (car x) (cdr x) 'font-lock-type-face msg))
	    (s-matched-positions-all "([^) ]+?)" msg 0))
      ))
  msg)

;; ======================================================
;; toggle excludeDecoration
;; ======================================================

(defun my-magit-toggle-exclude-decoration-in-git-config ()
  "Toggle the comment marks at BOL of `excludeDecoration = ...` in `CURRENT_REPO/.git/config`

This will toggle between:

  # excludeDecoration =

and

  excludeDecoration =
"
  (interactive)
  (let* ((git-root (locate-dominating-file "." ".git/config"))
	 (git-config (file-name-concat git-root ".git/config")))
    (with-temp-buffer
      (insert-file-contents git-config)
      (goto-char (point-min))
      (when (re-search-forward "\\[log\\]" nil t)
	(while (re-search-forward "\\([[:space:]]+\\)\\(# *\\)?\\(excludeDecoration *= *.+\\)" nil 'no-error)
	  (if (match-string 2)
	      (replace-match "\\1\\3")
	    (replace-match "\\1# \\3"))
	  )
	)
      (write-region (point-min) (point-max) git-config nil 'silent))
    )
  (magit-refresh)
  )

(with-eval-after-load 'magit-log
  (define-key magit-log-mode-map (kbd "M-E") #'my-magit-toggle-exclude-decoration-in-git-config)
  )


 (provide 'rc-magit)
;;; rc-magit.el ends here
