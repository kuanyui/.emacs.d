;;; emms-info-mediainfo.el --- Info-method for EMMS using medianfo

;; Copyright (C) 2013 Free Software Foundation, Inc.

;; Author: Fabián Ezequiel Gallina <fgallina@gnu.org>
;; Version: 0.1.0
;; Keywords: multimedia, processes
;; Package-Requires: ((emms "0"))

;; This file is NOT part of EMMS.

;; emms-info-mediainfo.el is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; emms-info-mediainfo.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with emms-info-mediainfo.el; see the file COPYING. If not,
;; write to the Free Software Foundation, Inc., 51 Franklin St, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; To activate this method for getting info, use something like:

;; (require 'emms-info-mediainfo)
;; (add-to-list 'emms-info-functions 'emms-info-mediainfo)

;;; Code:

(require 'emms-info)

(defgroup emms-info-mediainfo nil
  "An EMMS-info method for getting/setting ID3v1 tags, using the
external mediainfo program"
  :group 'emms-info)

(defcustom emms-info-mediainfo-program "mediainfo"
  "*The name/path of the mediainfo tag program."
  :type 'string
  :group 'emms-info-mediainfo)

(defcustom emms-info-mediainfo-arguments ""
  "The argument to pass to `emms-info-mediainfo-program'.
This should be a list of info-flag=value lines."
  :type '(repeat string)
  :group 'emms-info-mediainfo)

(defun emms-info-mediainfo-parse-playing-time (value)
  "Parse VALUE into seconds.
Playing time as returned by mediainfo normally looks like
something to \"1h 3mn\" or \"5mn 12s\", this converts these kind
of strings to the number of seconds they represent.  For example
the \"5mn 12s\" is converted to the integer 312."
  (let ((sum 0))
    (save-match-data
     (dolist (str (split-string value))
       (when (string-match "^\\([[:digit:]]\\{1,2\\}\\)\\(h\\|mn\\|s\\)?" str)
         (let ((val (string-to-number (match-string 1 str)))
               (unit (match-string 2 str)))
           (setq sum (+ sum (cond ((string= unit "h")
                                   (* val 60 60))
                                  ((string= unit "mn")
                                   (* val 60))
                                  ((string= unit "s")
                                   val)
                                  (t val))))))))
    sum))

(defun emms-info-mediainfo-parse-playing-time-miliseconds (value)
  "Parse VALUE into seconds.
Argument VALUE is assumed to be a string of the track's playing
time in miliseconds."
  (/ (string-to-number value) 1000))

(defun emms-info-mediainfo-parse-year (value)
  "Parse track's year from VALUE."
  (save-match-data
    (when (string-match "\\(\\b[[:digit:]]\\{4\\}\\b\\)" value)
      (match-string 1 value))))

(defun emms-info-mediainfo (track)
  "Add track information to TRACK.
This function is intended to be added to the
`emms-info-functions'.  Parses the output from
`emms-info-mediainfo-program' to gather most track details."
  (when (eq 'file (emms-track-type track))
    (with-temp-buffer
      (when (zerop
             (call-process
              (executable-find emms-info-mediainfo-program)
              nil t nil (emms-track-name track)))
        (dolist (property
                 `((info-artist "Performer")
                   (info-artist "TCOM")
                   (info-title "Track name")
                   (info-title "TIT2")
                   (info-album "Album")
                   (info-album "TALB")
                   (info-note "Comment")
                   (info-year
                    "Recorded date" ,#'emms-info-mediainfo-parse-year)
                   (info-year
                    "TDRC" ,#'emms-info-mediainfo-parse-year)
                   (info-year
                    "TYER" ,#'emms-info-mediainfo-parse-year)
                   (info-year
                    "TDAT" ,#'emms-info-mediainfo-parse-year)
                   (info-tracknumber "Track name/Position")
                   (info-tracknumber "TRCK")
                   (info-genre "Genre")
                   (info-playing-time
                    "Duration" ,#'emms-info-mediainfo-parse-playing-time)
                   (info-tracknumber
                    "TLEN"
                    ,#'emms-info-mediainfo-parse-playing-time-miliseconds)))
          (goto-char (point-min))
          (let* ((attribute (nth 0 property))
                 (str (nth 1 property))
                 (process-fn (or (nth 2 property) #'identity))
                 (foundp (re-search-forward
                          (concat "^" (regexp-quote str)
                                  "[[:space:]]+:[[:space:]]+\\([^\n]+\\)") nil t))
                 (value (and foundp (match-string-no-properties 1))))
            (when (and (not (emms-track-get track attribute))
                       (> (length value) 0))
              (emms-track-set track attribute (funcall process-fn value)))))))))

(provide 'emms-info-mediainfo)
;;; emms-info-mediainfo.el ends here
