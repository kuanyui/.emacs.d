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

(defun my-git-get-repo-paths (from-path)
  "Returns a plist"
  (let* ((parent-dir (or (locate-dominating-file from-path ".git") ""))
	 (git-file-or-dir (file-name-concat parent-dir ".git")))
    (cond ((null parent-dir)
	   '())
	  ((file-regular-p git-file-or-dir)  ; A .git file
	   (let* ((git-file-content (with-temp-buffer (insert-file-contents git-file-or-dir) (buffer-string)))
		  (git-dir-relpath (progn
				     (string-match "gitdir: *\\(.+\\)" git-file-content)
				     (match-string 1 git-file-content)))
		  (git-dir-abspath (file-truename (file-name-concat parent-dir git-dir-relpath))))  ; Note: file-truename chases symlink.
	     (list :type 'submodule :project-dir parent-dir :git-dir git-dir-abspath :git-config (file-name-concat git-dir-abspath "config"))))
	  ((file-directory-p git-file-or-dir)
	   (list :type 'regular :project-dir parent-dir :git-dir git-file-or-dir :git-config (file-name-concat git-file-or-dir "config")))
	  )
    ))
;; TESTS:
;; (my-git-get-repo-paths "../git/source/recentz")
;; (my-git-get-repo-paths ".")

(defun my-magit-find-file-git-config ()
  (interactive)
  (let ((pathes (my-git-get-project-root-dir ".")))
    (if (not pathes)
	(message "Not inside a git repo. Aborted.")
      (find-file (plist-get pathes :git-config)))))


(defun my-magit-toggle-exclude-decoration-in-git-config ()
  "Toggle the comment marks at BOL of `excludeDecoration = ...` in `CURRENT_REPO/.git/config`

This will toggle between:

  # excludeDecoration =

and

  excludeDecoration =
"
  (interactive)
  (let* ((git-config (plist-get (my-git-get-repo-paths ".") :git-config)))
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

(with-eval-after-load 'magit-status
  (define-key magit-hunk-section-map (kbd "M-U") #'magit-smerge-keep-upper)
  (define-key magit-hunk-section-map (kbd "M-L") #'magit-smerge-keep-lower)
  )

;; ======================================================
;; Copy branch + tag for quickly pasting to issue tracker
;; ======================================================

(defun my-git-branch-contains (commit-hash)
  (let* ((raw (shell-command-to-string (format "git branch --contains %s" commit-hash)))
	 (lines (string-split (string-trim-right raw) "\n"))
	 (lines (mapcar (lambda (s) (substring s 2)) lines))
	 (lines (cl-delete "" lines :test #'equal)))
    lines
    ))

(defun my-git-nearest-tags (commit-hash)
  (let* ((raw (shell-command-to-string (format "git describe --long --tags %s" commit-hash)))
	 (oldest-tagname (save-match-data   ; the oldest tag on the nearest commit which has at least one tag.
			   (string-match "\\(.+\\)-[0-9]+-[a-zA-Z0-9]+$" raw)
			   (match-string 1 raw)))
	 (tags (string-split (shell-command-to-string (format "git tag --points-at \"%s\"" oldest-tagname)))))
    (reverse tags)
    ))

(defun my-magit-log-copy-branch-tag-info-for-issue ()
  (interactive)
  (let* ((commit-hash (magit-commit-p (magit-commit-at-point)))
         (tags (my-git-nearest-tags commit-hash))
 	 (branches (my-git-branch-contains commit-hash))
	 (tag (or (magit-tag-at-point)
		  (and (> (length tags) 0)
		       (ido-completing-read "Tag: " tags nil t))))
 	 (branch (or (magit-branch-at-point)
		     (if (= (length branches) 1) (car branches))
		     (ido-completing-read "Branch: " branches nil t)))
	 (msg (format "This issue is fixed at a commit (`%s`) in branch `%s`, and should be available in versions **newer than** `%s`" commit-hash branch tag)))
    (message "%S" msg)))

(with-eval-after-load 'magit-log
  (define-key magit-log-mode-map (kbd "C-x g i") #'my-magit-log-copy-branch-tag-info-for-issue)
  )


(provide 'rc-magit)
;;; rc-magit.el ends here
