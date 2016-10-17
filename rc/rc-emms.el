;;; rc-emms.el ---                                   -*- lexical-binding: t; -*-


;; ======================================================
;; Requirement: mpv, mp3info
;; ======================================================

(require 'dired)
(require 'emms-setup)
(emms-all)
(emms-standard)
(setq emms-source-file-default-directory "~/multimedia/")

(setq emms-repeat-playlist t)
(setq emms-seek-seconds 3)

;; ======================================================
;; Auto save playlist when exit emacs, and load automatically when startup
;; ======================================================
(defvar emms-auto-save-playlist-path (concat emms-directory "/" "auto-save.m3u"))

(global-set-key (kbd "C-c e") 'my-emms)

(defun my-emms ()
  (interactive)
  (if (or (null emms-playlist-buffer)
          (not (buffer-live-p emms-playlist-buffer)))
      ;; (emms-history-load)  ;; fuck you useless history-load, broken shit.
      (emms-add-m3u-playlist emms-auto-save-playlist-path)
    )
  (emms-playlist-mode-go))

(add-hook 'kill-emacs-hook
          (lambda ()
            (if emms-playlist-buffer
                (emms-playlist-save 'm3u emms-auto-save-playlist-path)
              )))


;; ======================================================
;; Progress bar when seeking
;; ======================================================
(defun my-emms-indicate-seek (_shit)
  (let* ((total-playing-time (emms-track-get
                              (emms-playlist-current-selected-track)
                              'info-playing-time))
         (elapsed/total (/ (* 100 emms-playing-time) total-playing-time)))
    (with-temp-message (format "[%-100s] %2d%%"
                               (make-string elapsed/total ?=)
                               elapsed/total)
      (sit-for 2))))

(add-hook 'emms-player-seeked-functions #'my-emms-indicate-seek)


;; ======================================================
;; Use Mediainfo to get info
;; ======================================================

(require 'emms-info-mediainfo)
(add-to-list 'emms-info-functions #'emms-info-mediainfo)

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

(define-key dired-mode-map (kbd "a") 'dired-add-to-emms-playlist)


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
  (dired-next-line 1))


(defun file-audio-or-video-p (file-path)
  (let* ((safe-path (replace-regexp-in-string "\"" "\\\"" (expand-file-name file-path)))
         (mime (shell-command-to-string (format "file --mime --brief \"%s\"" safe-path)))
         (type (car (split-string mime "/"))))
    (if (member type '(video audio))
        type
      nil)))


;; ======================================================
;; Playlist format: I hate ID3Tag, but don't show full path neither.
;; ======================================================
;;(setq emms-playlist-insert-track-function 'emms-playlist-simple-insert-track) ; unchanged
;;(setq emms-track-description-function 'emms-info-track-description) ; unchanged
(setq emms-track-description-function 'my-emms-get-track-one-line-info)
(defun my-emms-get-track-one-line-info (track)
  (let ((artist (emms-track-get track 'info-artist))
        (year (emms-track-get track 'info-year))
        (album (emms-track-get track 'info-album))
        (tracknumber (emms-track-get track 'info-tracknumber))
        (title (emms-track-get track 'info-title)))
    ;; Considering removing this. I hate ID3 tag.
    (if (or artist title)
        (mapconcat #'identity
                   (remove nil (list
                                (if (> (length tracknumber) 0)
                                    (format "%02d" (string-to-number tracknumber))
                                  nil)
                                (if (> (length artist) 0) artist nil)
                                (if (> (length year) 0) year nil)
                                (if (> (length album) 0) album nil)
                                (if (> (length title) 0) title nil)))
                   "/")
      ;; if no ID3 tag found
      (let* ((fullpath (emms-track-get track 'name))
             (splitted-fullpath (split-string fullpath "/"))
             (len (length splitted-fullpath)))
        (if (<= len 5)
            (file-name-base fullpath)
          (string-join (nthcdr (- len 2) splitted-fullpath) "/"))))))

;; ======================================================
;; Mode-line
;; ======================================================

;; Show only file name, instead of full file path
(defadvice emms-track-description (after show-only-filename activate)
  (setq ad-return-value
        (string-trim (file-name-base ad-return-value))))


;; Replace `emms-mode-line-icon-function'
;; (Remove "NP:")
(setq emms-mode-line-mode-line-function 'my-emms-mode-line-icon-function)
(defun my-emms-mode-line-icon-function ()
  (concat ""
          emms-mode-line-icon-before-format
          (emms-propertize "" 'display emms-mode-line-icon-image-cache)
          (emms-mode-line-playlist-current)))


;; Format
(setq emms-playing-time-display-format "(%s)")
(setq emms-mode-line-format "[%s]")

(emms-mode-line -1)
(emms-playing-time -1)

;; emms-state.el
(require 'emms-state)
(setq emms-state-stop "#")
(setq emms-state-play ">")
(setq emms-state-pause "||")
(setq emms-state-mode-line-string
      '("" emms-state ""
        (emms-state-current-playing-time
         (:propertize emms-state-current-playing-time
                      face emms-state-current-playing-time))
        (emms-state-total-playing-time
         ("/"
          (:propertize emms-state-total-playing-time
                       face emms-state-total-playing-time)
          ))
        emms-mode-line-string))

(emms-state-mode)

;; ======================================================
;; Playlist move up/down
;; ======================================================
;; [TODO]


(provide 'rc-emms)
;;; rc-emms.el ends here
