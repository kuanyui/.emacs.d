;;; rc-emms.el ---                                   -*- lexical-binding: t; -*-


;; ======================================================
;; Requirement: mpv, mp3info
;; ======================================================

(require 'dired)
(require 'emms-setup)
(emms-all)
(emms-standard)
(setq emms-source-file-default-directory "~/multimedia/")

;; ======================================================
;; Auto save play list when exit emacs
;; ======================================================


(global-set-key (kbd "C-c e") 'my-emms)

(defun my-emms ()
  (interactive)
  (if (or (null emms-playlist-buffer)
          (not (buffer-live-p emms-playlist-buffer)))
      (emms-history-load))
  (emms-playlist-mode-go))

(add-hook 'kill-emacs-hook (lambda () (if emms-playlist-buffer (emms-history-save))))


;; ======================================================
;; use MPV as backend
;; ======================================================
(define-emms-simple-player mpv '(file url streamlist playlist)
  "" ;; no regexp
  "mpv" "--no-video" "--quiet" "--really-quiet")

(require 'emms-player-mpv)
(setq emms-player-list '(emms-player-mpv))

;; ======================================================
;; Add file to playlist from Dired
;; ======================================================

(define-key dired-mode-map (kbd "M-a") 'dired-add-to-emms-playlist)


;; Disable video output to prevent a stupid new window.
(defun dired-add-to-emms-playlist ()
  (interactive)
  (let ((file-path (dired-get-filename)))
    (if (or (member (file-name-extension file-path)
                    '("ogg" "mp3" "wav" "mpg" "mpeg" "wmv" "wma"
                      "mov" "avi" "divx" "ogm" "ogv" "asf" "mkv"
                      "rm" "rmvb" "mp4" "flac" "vrob" "m4a" "ape"
                      "flv" "webm"))
            (file-audio-or-video-p file-path))
        (emms-add-dired)))
  (next-line))


(defun file-audio-or-video-p (file-path)
  (let* ((safe-path (replace-regexp-in-string "\"" "\\\"" (expand-file-name file-path)))
         (mime (shell-command-to-string (format "file --mime --brief \"%s\"" safe-path)))
         (type (car (split-string mime "/"))))
    (if (member type '(video audio))
        type
      nil)))


;; ======================================================
;; Mode-line
;; ======================================================

;; Show only file name, instead of full file path
(defadvice emms-track-description (after show-only-filename activate)
  (setq ad-return-value
        (substring (file-name-base ad-return-value) 1)))


;; Replace `emms-mode-line-icon-function'
;; (Remove "NP:")
(setq emms-mode-line-mode-line-function 'my-emms-mode-line-icon-function)
(defun my-emms-mode-line-icon-function ()
  (concat ""
          emms-mode-line-icon-before-format
          (emms-propertize "" 'display emms-mode-line-icon-image-cache)
          (emms-mode-line-playlist-current)))


(setq emms-playing-time-display-format "(%s)")
(setq emms-mode-line-format "[%s]")

(emms-mode-line 1)
(emms-playing-time 1)

;; (require 'emms-state)

;; (setq emms-state-stop "x")
;; (setq emms-state-play ">")
;; (setq emms-state-pause "||")

;; (setq emms-state-mode-line-string
;;       '("" emms-state " "
;;         (emms-state-current-playing-time
;;          (:propertize emms-state-current-playing-time
;;                       face emms-state-current-playing-time))
;;         (emms-state-total-playing-time
;;          ("("
;;           (:propertize emms-state-total-playing-time
;;                        face emms-state-total-playing-time)
;;           ")"))
;;         emms-mode-line-string))

;; (emms-state-mode)


(provide 'rc-emms)
;;; rc-emms.el ends here
