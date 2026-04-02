;;; rc-dired.el ---                                  -*- lexical-binding: t; -*-

;;======================================================
;; Dired
;;======================================================

;; ======================================================
;; Dired+
;; ======================================================
;; Lazy-load Dired+
(with-eval-after-load 'dired
  (add-to-list 'find-directory-functions 'my-dired-noselect)
  (defun my-dired-noselect (dir-path)
    "Make Dired+ can be lazy-loaded before the first-time opening of dired buffer"
    ;; (require 'dired+)
    (dired-noselect dir-path)
    )
  (require 'dired-aux)
  ;;(require 'dired-async)
  (diredfl-global-mode 1)
  (setq dired-dwim-target t)

  ;; Human-readable size
  (setq dired-listing-switches "-alh")

  ;; ======================================================
  ;; SELinux
  ;; ======================================================
  (when (and (eq system-type 'gnu/linux)
	     (file-exists-p "/sys/fs/selinux/enforce"))  ;; Check whether SELinux install or not.
    (setq dired-listing-switches "-alhZ")
    (defun my-dired-selinux-highlight ()
      "Highlight for SELinux context (u:r:t)"
      (font-lock-add-keywords
       nil				; apply on current buffer only
       '(("\\([a-zA-Z_0-9]+_u\\):\\([a-zA-Z_0-9]+_r\\):\\([a-zA-Z_0-9]+_t\\)"
	  ;; Use `:override' to force override `diredfl-global-mode'.
	  (1 '(:inherit font-lock-keyword-face :override t))       ; User
	  (2 '(:inherit font-lock-function-name-face :override t)) ; Role
	  (3 '(:inherit font-lock-type-face :override t))          ; Type
	  ))
       'append)  ; Append after default rules of Dired
      )
    ;; (add-hook 'dired-mode-hook 'my-dired-selinux-highlight)  ;; Useless when `diredfl-global-mode' is enabled.
    (add-hook 'dired-after-readin-hook #'my-dired-selinux-highlight t)
    )

  ;; ======================================================
  ;; Open with External Program
  ;; ======================================================
  (defun system-open-command ()
    "M-RET to call `kde-open` to open file."
    (or
     (executable-find "xdg-open")
     (executable-find "kde-open")
     (executable-find "open")))

  (defun get-open-program (filename)
    (let ((ext (downcase (file-name-extension filename)))
	  (exe-tester (if (eq system-type 'darwin)
			  (lambda (app) (eq 0 (call-process (format "open") nil nil t "-Ra" "XnViewMP")))
			#'executable-find
			)))
      (cond ((member ext '("jpg" "jpeg" "png" "gif"))
	     (find-if exe-tester '("XnViewMP")))
	    ((member ext '("mov" "mp4" "mp3" "mkv" "avi" "flv"))
	     (find-if exe-tester '("mpv" "vlc")))
	    )))
  ;; (get-open-program "test.mp4")


  (defun dired-open-file-with-external-program ()
    "Open file with external program in dired"
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (message "Opening %s..." file)
      (call-process (system-open-command) nil 0 nil file)
      (message "Opening %s done" file)))


  (defun open-current-directory-with-external-program ()
    "Open current directory with external program."
    (interactive)
    (call-process (system-open-command) nil 0 nil (file-truename default-directory)))
  (define-key dired-mode-map (kbd "C-x C-j") 'open-current-directory-with-external-program)

  (define-key dired-mode-map (kbd "M-RET") 'dired-open-file-with-external-program)
  (define-key dired-mode-map (kbd "C-M-j") 'dired-open-file-with-external-program)


  ;; dired hide/show detail infomation
  ;; [FIXME] Emacs 24.4 will build-in `dired-hide-details-mode'.
  ;; (require 'dired-details)
  ;; (dired-details-install)

  ;; 'always means no asking; 'top means ask once
  (setq dired-recursive-copies 'top)
  (setq dired-recursive-deletes 'top)

  ;; 回到上層目錄後，自動把cursor移動到前一個目錄處
  (defun my-dired-backward ()
    "Go back to the parent directory (..), and the cursor will be moved to where
the previous directory."
    (interactive)
    (let* ((DIR (or (uniquify-buffer-base-name)
		    (buffer-name))))
      (if (equal DIR "*Find*")
	  (quit-window t)
	(progn (find-alternate-file "..")
	       (goto-char (point-min))
	       (while (and (not (eobp))
			   (not (equal (dired-get-filename t :no-error) DIR)))
		 (next-line))
	       ;;(revert-buffer)
	       ))))


  ;; 按Enter開檔案時Dired時不會一直開新的Dired buffer（按Enter時只用同一個Dired開檔案）
  (defun dired-my-find-alternate-file ()
    (interactive)
    (if (file-regular-p (dired-get-filename))
	(dired-find-file)
      (dired-find-alternate-file)))
  (define-key dired-mode-map (kbd "RET") 'dired-my-find-alternate-file) ; 按Enter開檔案
  (put 'dired-find-alternate-file 'disabled nil) ; 避免Dired問你一些囉唆的問題
  (define-key dired-mode-map (kbd "q") 'my-dired-backward)  ; 按q回到上層目錄

  ;; [FIXME] 我忘記這是啥東西了...
  ;; (put 'dired-find-alternate-file 'disabled nil)

  ;; 搜尋目前目錄
  (define-key dired-mode-map "f" 'dired-find-name-in-current-directory)
  (define-key dired-mode-map "F" 'dired-find-name-in-current-directory)
  (defun dired-find-name-in-current-directory ()
    (interactive)
    (find-name-dired default-directory
		     (format "*%s*" (read-from-minibuffer "Pattern: ")))
    (set-buffer-multibyte t))
  ;;(setq find-ls-option '("-print" . ""))
  (setq find-name-arg "-iname")

  ;; 修正*Find*裡的中文亂碼問題
  (setq find-ls-option '("-print0 | xargs -0 ls -ald" . ""))

  ;;hide didden file
  (require 'dired-x)
  (setq dired-omit-files "^\\...+$")

  ;; Dired Omit 加強
  (defvar v-dired-omit t
    "If dired-omit-mode enabled by default. Don't setq me.")
  (defun dired-omit-switch ()
    "This function is a small enhancement for `dired-omit-mode', which will
\"remember\" omit state across Dired buffers."
    (interactive)
    (if (eq v-dired-omit t)
	(setq v-dired-omit nil)
      (setq v-dired-omit t))
    (dired-omit-caller)
    (revert-buffer))
  (defun dired-omit-caller ()
    (if v-dired-omit
	(setq dired-omit-mode t)
      (setq dired-omit-mode nil)))
  (define-key dired-mode-map (kbd "C-x M-o") 'dired-omit-switch)
  (add-hook 'dired-mode-hook 'dired-omit-caller)


  ;;sort directories first
  (defun dired-directory-sort ()
    "Dired sort hook to list directories first."
    (save-excursion
      (let (buffer-read-only)             ; 原來解除read-only是這樣寫的OAO...
	(forward-line 2) ;; beyond dir. header
	(sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
    (and (featurep 'xemacs)
	 (fboundp 'dired-insert-set-properties)
	 (dired-insert-set-properties (point-min) (point-max)))
    (set-buffer-modified-p nil))

  (add-hook 'dired-after-readin-hook 'dired-directory-sort)

  ;; Sort files!
  ;; 按 s 排序檔案，會先問你要根據什麼屬性排序，而且紀錄下排序狀態，不會
  ;; 跨 buffer 就不見了。
  (defun dired-sort-size ()
    "Dired sort by size."
    (interactive) (dired-sort-other (concat dired-listing-switches "S")))
  (defun dired-sort-extension ()
    "Dired sort by extension."
    (interactive) (dired-sort-other (concat dired-listing-switches "X")))
  (defun dired-sort-ctime ()
    "Dired sort by create time."
    (interactive) (dired-sort-other (concat dired-listing-switches "ct")))
  (defun dired-sort-utime ()
    "Dired sort by access time."
    (interactive) (dired-sort-other (concat dired-listing-switches "ut")))
  (defun dired-sort-time ()
    "Dired sort by time."
    (interactive) (dired-sort-other (concat dired-listing-switches "t")))
  (defun dired-sort-name ()
    "Dired sort by name."
    (interactive) (dired-sort-other (concat dired-listing-switches "")))

  (defvar v-dired-sort 'name)
  (defun dired-sort-and-remember ()
    ""
    (interactive)
    (setq v-dired-sort
	  (intern
	   (completing-read "Sort by: " '(name size extension ctime utime time) nil t
			    (cond ((eq v-dired-sort 'name) "time")
				  ((eq v-dired-sort 'time) "name")
				  ((eq v-dired-sort 'size) "name")
				  (t nil)))))
    (dired-sort-auto-apply))
  (defun dired-sort-auto-apply ()
    (cond ((eq v-dired-sort 'name) (dired-sort-name))
	  ((eq v-dired-sort 'size) (dired-sort-size))
	  ((eq v-dired-sort 'extenstion) (dired-sort-extenstion))
	  ((eq v-dired-sort 'ctime) (dired-sort-ctime))
	  ((eq v-dired-sort 'utime) (dired-sort-utime))
	  ((eq v-dired-sort 'time) (dired-sort-time))))
  (add-hook 'dired-mode-hook 'dired-sort-auto-apply)
  (define-key dired-mode-map "s" 'dired-sort-and-remember)


  ;; Filename filter
  ;; (Just like C-i in KDE Dolphin)
  (defun dired-show-only (regexp)
    (interactive "sFiles to show (regexp): ")
    (dired-mark-files-regexp regexp)
    (dired-toggle-marks)
    (dired-do-kill-lines))
  (define-key dired-mode-map (kbd "C-i") 'dired-show-only)

  (defun dired-open-mounted-media-dir ()
    (interactive)
    (find-file "/var/run/media/"))
  (define-key dired-mode-map (kbd "C-c m m") 'dired-open-mounted-media-dir)

  (defun dired-add-to-smplayer-playlist ()
    "Add a multimedia file or all multimedia files under a directory into SMPlayer's playlist via Dired."
    (interactive)
    (require 'cl-lib)
    (let* ((PATTERN "\\(\\.mp4\\|\\.flv\\|\\.rmvb\\|\\.mkv\\|\\.avi\\|\\.rm\\|\\.mp3\\|\\.wav\\|\\.wma\\|\\.m4a\\|\\.mpeg\\|\\.aac\\|\\.ogg\\|\\.flac\\|\\.ape\\|\\.mp2\\|\\.wmv\\|.m3u\\|.webm\\|.3gpp\\)$")
	   (FILE (dired-get-filename nil t)))
      (if (file-directory-p FILE)    ;if it's a dir.
	  (let* ((FILE_LIST (directory-files FILE t PATTERN))
		 (n 0)
		 s_FILE_LIST)
	    (dolist (x FILE_LIST)
	      (if (not (or (equal x ".") (equal x "..")))
		  (setq s_FILE_LIST (concat s_FILE_LIST "'" x "' ")))
	      (setq n (1+ n)))
	    (message "Opening %s files..." n)
	    (call-process-shell-command "smplayer -add-to-playlist" nil nil nil (format "%s &" s_FILE_LIST)))
	(if (string-match PATTERN FILE)    ;if it's a file
	    (call-process "smplayer" nil 0 nil "-add-to-playlist" FILE)
	  (message "This is not a supported audio or video file."))))
    (dired-next-line 1))

  (define-key dired-mode-map (kbd "M-a") 'dired-add-to-smplayer-playlist)
  (define-key dired-mode-map (kbd "<f2>") 'wdired-change-to-wdired-mode)


  (defun dired-tar (tarname files &optional arg)
    "A dired-mode extension to archive files marked.
With one prefix argument, the tarball is gziped."
    (interactive (let ((files (dired-get-marked-files)))
		   (list (read-string "Tarball name: "
				      (concat (file-relative-name (car files)) ".tar.gz"))
			 files "P")))
    (let ((tar (if arg
		   (if dired-guess-shell-gnutar
		       (concat dired-guess-shell-gnutar " zcf %s %s")
		     "tar cf - %2s | gzip > %1s")
		 "tar cf %s %s")))
      (shell-command (format tar tarname (mapconcat 'file-relative-name files " ")))))
  (add-hook 'dired-load-hook (lambda () (define-key dired-mode-map "T" 'dired-tar)))
  )


(provide 'rc-dired)
;;; rc-dired.el ends here
