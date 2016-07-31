;;; emms-player-mpv.el --- mpv support for EMMS

;; Copyright (C) 2013-2016 ZHANG Weiyi
;; Copyright (C) 2014 Alex Kost

;; Authors: ZHANG Weiyi <dochang@gmail.com>,
;;          Alex Kost <alezost@gmail.com>
;; Version: 0.0.9
;; Package-Version: 20160621.1100
;; Package-Requires: ((emms "0"))
;; Keywords: emms, mpv
;; URL: https://github.com/dochang/emms-player-mpv/

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This provides a player that uses mpv. It supports pause and
;; seeking. For loading subtitles automatically, try adding
;; "autosub-match=fuzzy" to your `~/.mpv/config', see mpv manual for
;; more.
;;
;; This file is based on `emms-player-mplayer.el' in EMMS.

;;; Code:

(require 'emms-compat)
(require 'emms-player-simple)

(defcustom emms-player-mpv-input-file
  (expand-file-name (locate-user-emacs-file "emms-mpv-input-file"))
  "The file to send command to mpv."
  :type 'file
  :group 'emms)

(define-emms-simple-player mpv '(file url streamlist playlist)
  (concat "\\`\\(http\\|mms\\)://\\|"
          (emms-player-simple-regexp
           "ogg" "mp3" "wav" "mpg" "mpeg" "wmv" "wma"
           "mov" "avi" "divx" "ogm" "ogv" "asf" "mkv"
           "rm" "rmvb" "mp4" "flac" "vob" "m4a" "ape"
           "flv" "webm"))
  "mpv" "--quiet" "--really-quiet")

(defadvice emms-player-mpv-start (around append-arguments activate)
  (unless (file-exists-p emms-player-mpv-input-file)
    (call-process "mkfifo" nil nil nil emms-player-mpv-input-file))
  (let* ((input-file (format "--input-file=%s" emms-player-mpv-input-file))
         (track-arg (let* ((track (ad-get-arg 0))
                       (track-type (emms-track-get track 'type))
                       (track-name (emms-track-name track)))
                  (if (memq track-type '(streamlist playlist))
                      (format "--playlist=%s" track-name)
                    track-name)))
         (process (apply 'start-process
                         emms-player-simple-process-name
                         nil
                         emms-player-mpv-command-name
                         (append emms-player-mpv-parameters
                                 (list input-file track-arg)))))
    (set-process-sentinel process 'emms-player-simple-sentinel))
  (emms-player-started emms-player-mpv))

(emms-player-set emms-player-mpv
		 'pause
		 'emms-player-mpv-pause)

(emms-player-set emms-player-mpv
                 'resume
                 'emms-player-mpv-resume)

(emms-player-set emms-player-mpv
		 'seek
		 'emms-player-mpv-seek)

(emms-player-set emms-player-mpv
		 'seek-to
		 'emms-player-mpv-seek-to)

(defun emms-player-mpv--format-command (fmt &rest args)
  "Generate shell command to control mpv."
  (let ((mpv-cmd (apply 'format fmt args)))
    (format "echo %s > %s"
            (shell-quote-argument mpv-cmd)
            (shell-quote-argument emms-player-mpv-input-file))))

(defun emms-player-mpv-pause ()
  "Depends on mpv's --input-file option."
  (let ((cmd (emms-player-mpv--format-command "set pause yes")))
    (call-process-shell-command cmd nil nil nil)))

(defun emms-player-mpv-resume ()
  "Depends on mpv's --input-file option."
  (let ((cmd (emms-player-mpv--format-command "set pause no")))
    (call-process-shell-command cmd nil nil nil)))

(defun emms-player-mpv-seek (sec)
  "Depends on mpv's --input-file option."
  (let ((cmd (emms-player-mpv--format-command "seek %d" sec)))
    (call-process-shell-command cmd nil nil nil)))

(defun emms-player-mpv-seek-to (sec)
  "Depends on mpv's --input-file option."
  (let ((cmd (emms-player-mpv--format-command "seek %d absolute" sec)))
    (call-process-shell-command cmd nil nil nil)))

(provide 'emms-player-mpv)
;;; emms-player-mpv.el ends here
