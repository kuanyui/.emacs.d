;;; rc-twittering.el ---                             -*- lexical-binding: t; -*-

;;======================================================
;;Twittering-mode:用Emacs上Twitter
;;======================================================
(add-to-list 'load-path "~/.emacs.d/lisps/twittering-mode/")
(require 'twittering-mode)
(setq twittering-use-master-password t) ;;This requires GnuPG. And also, either EasyPG or alpaca.el (0.13) is necessary.
(twittering-enable-unread-status-notifier) ;;顯示未讀訊息數
;;(setq-default twittering-icon-mode t) ;;預設顯示頭像

;;開啟自己的favorite timeline
(defun my-twittering-favorites-timeline ()
  (interactive)
  (twittering-visit-timeline ":favorites"))

;;(defun twittering-mode-hook-func ()
;;  (set-face-bold-p 'twittering-username-face t)
;;  (set-face-foreground 'twittering-username-face "SkyBlue3"))
;;(add-hook 'twittering-mode-hook 'twittering-mode-hook-func)


(define-key twittering-mode-map (kbd "r") 'twittering-toggle-or-retrieve-replied-statuses)
(define-key twittering-mode-map (kbd "R") 'twittering-replies-timeline)
(define-key twittering-mode-map (kbd "U") 'twittering-user-timeline)
(define-key twittering-mode-map (kbd "W") 'twittering-update-status-interactive)
(define-key twittering-mode-map (kbd "O") 'twittering-organic-retweet)
(define-key twittering-mode-map (kbd "N") 'twittering-native-retweet)
(define-key twittering-mode-map (kbd "H") 'twittering-home-timeline)
(define-key twittering-mode-map (kbd "F") 'twittering-favorite)
(define-key twittering-mode-map (kbd "V") 'twittering-visit-timeline)
(define-key twittering-mode-map (kbd "M") 'my-twittering-favorites-timeline)
(define-key twittering-mode-map (kbd "M-w") 'twittering-push-tweet-onto-kill-ring)
(define-key twittering-mode-map (kbd "C-w") 'twittering-push-uri-onto-kill-ring)
(define-key twittering-mode-map (kbd "D") 'twittering-direct-messages-timeline)
(define-key twittering-mode-map (kbd "S") 'twittering-sent-direct-messages-timeline)
(define-key twittering-mode-map (kbd "q") 'twittering-bury-main-timeline-buffer)
(define-key twittering-mode-map (kbd "[") 'twittering-switch-to-previous-timeline)
(define-key twittering-mode-map (kbd "]") 'twittering-switch-to-next-timeline)

(defun twittering-bury-main-timeline-buffer ()
  "If in main timeline buffer (:home), bury-buffer.
If not, kill-buffer instead. "
  (interactive)
  (if (and (or (equal (buffer-name) ":home")
               (equal (buffer-name) ":direct_messages")
               (equal (buffer-name) ":direct_messages_sent")
               (equal (buffer-name) ":replies"))
           (eq major-mode 'twittering-mode))
      (bury-buffer)
    (kill-buffer)))
;;讓twittering-status-buffer支援換行
(setq twittering-status-format
      "%i %s,%p %@:
%FOLD[  ]{%T // from %f%L%r%R}
 ")

(setq twittering-retweet-format
      '(nil _ " RT: %t (via @%s)")
      )

;; [FIXME] twittering-update-status沒有hook可用，看看要不要自己定義一個發推用function，可以把發出的推也一起加入diary.org的結尾。

;;類似pentadactyl按[f]後輸入數字開啟連結
(autoload 'twittering-numbering "twittering-numbering" nil t)
(add-hook 'twittering-mode-hook 'twittering-numbering)


(add-hook 'twittering-mode-hook 'auto-fill-mode)

;;;; Filtering for Tweets
(defvar twittering-filter-users '()
  "*List of strings containing usernames (without '@' prefix) whose tweets should not be displayed in timeline.")
(defvar twittering-filter-tweets '()
  "*List of strings containing phrases which will prevent a tweet containing one of those phrases from being displayed in timeline.")

(defun twittering-filters-apply ()
  (setq non-matching-statuses '())
  (dolist (status twittering-new-tweets-statuses)
    (setq matched-tweets 0)
    (dolist (pat twittering-filter-users)
      (if (string-match pat (cdr (assoc 'user-screen-name status)))
          (setq matched-tweets (+ 1 matched-tweets))))
    (dolist (pat twittering-filter-tweets)
      (if (string-match pat (twittering-make-fontified-tweet-text-with-entity status))
          (setq matched-tweets (+ 1 matched-tweets))))
    (if (= 0 matched-tweets)
        (setq non-matching-statuses (append non-matching-statuses `(,status)))))
  (setq new-statuses non-matching-statuses))

(add-hook 'twittering-new-tweets-hook 'twittering-filters-apply)

(setq twittering-filter-tweets
      '(
	"http://4sq.com/.*"
	"http://adf.ly/.*"
	"I liked a @YouTube video"
	"我喜歡一部 .*@YouTube 影片"
	"中時"
	"郭董"
	"nikeplus"
	"采潔"
	"連勝文"
	"神豬"
	"連D"
	"蔡正元"
	"星球與廣大的銀河系！"
	))

(defalias 'short-url 'twittering-tinyurl-replace-at-point)
;;高亮特定使用者，但搞不出來先擺著。
;;(defface twittering-star-username-face
;;  `((t (:underline t :foreground "a40000" :background "#ffaf87"))) "" :group 'faces)
;;(font-lock-add-keywords 'twittering-mode
;;
;;                        '(("jserv" 0 'twittering-star-username-face)))
;;
;;(defface twittering-keyword-face
;;  `((t (:underline t :foreground "a40000"))) "" :group 'faces)
;;
;;(font-lock-add-keywords 'twittering-mode
;;                        '(("keyword" 0 'twittering-keyword-face)))


;;(assq 'text (twittering-find-status (twittering-get-id-at)))

(add-hook 'twittering-new-tweets-hook 'twittering-my-notification)
(defun twittering-my-notification ()
  (if (string=
       (twittering-timeline-spec-to-string
        twittering-new-tweets-spec)
       ":replies")
      (let ((n twittering-new-tweets-count))
        (start-process "twittering-notify" nil "notify-send"
                       "-i" "~/.emacs.d/icon.png"
                       "New tweets"
                       (format "You have %d new tweet%s"
                               n (if (> n 1) "s" ""))))))


(load-file "~/.emacs.d/git/twittering-myfav/twittering-myfav.el")
(require 'twittering-myfav)
(setq twittering-myfav-file-name "twittering_myfav") ; The org and html file's name.
(setq twittering-myfav-file-path "~/Dropbox/Blog/kuanyui.github.io/source/") ; remember "/" in the end
(setq twittering-myfav-your-username "azazabc123") ; without "@"
(define-key twittering-mode-map (kbd "A") 'twittering-myfav-add-to-file)

(defun twittering-myfav-export-to-hexo ()
  (interactive)
  (twittering-myfav-export-to-html)
  (write-file "~/Dropbox/Blog/kuanyui.github.io/source/twittering_myfav.html" nil)
  (goto-char (point-min))
  (insert "layout: false\n---\n\n")
  (save-buffer))

(define-key twittering-edit-mode-map (kbd "<f4>") 'ispell-word)
(define-key twittering-edit-mode-map (kbd "C-x <f4>") 'twittering-edit-replace-at-point)


(provide 'rc-twittering)
;;; rc-twittering.el ends here
