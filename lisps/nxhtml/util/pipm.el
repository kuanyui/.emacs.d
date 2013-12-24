;;; pipm.el --- Compare journal list for PsychInfo and Pubmed
;; 
;; Filename: pipm.el
;; Description: 
;; Author: 
;; Maintainer: 
;; Created: Wed Sep  7 04:04:27 2011 (+0200)
;; Version: 
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(defvar base-dir "c:/Users/Lennart/Downloads/")
(defvar j-medline-txt (expand-file-name "J_Medline.txt" base-dir))
(defvar j-medline-nrm (expand-file-name "J_Medline.nrm" base-dir))
(defvar psychinfo-csv (expand-file-name "psychinfo-journalcovlist.csv" base-dir))
(defvar psychinfo-nrm (expand-file-name "psychinfo-journalcovlist.nrm" base-dir))
(defvar psychinfo-more (expand-file-name "psychinfo-not-pubmed.txt" base-dir))

(when nil (pipm-pubmed-normalize j-medline-txt j-medline-nrm))
(when nil (pipm-psychinfo-normalize psychinfo-csv psychinfo-nrm))
(when nil (pipm-psychinfo-not-pubmed))

(defun pipm-psychinfo-not-pubmed ()
  (let ((psy-buf (find-file-noselect psychinfo-nrm))
        (jmd-buf (find-file-noselect j-medline-nrm))
        (out-buf (find-file-noselect psychinfo-more))
        )
    (with-current-buffer out-buf
      (unless (= 0 (buffer-size))
        (error "not empty")))
    (with-current-buffer jmd-buf
      (widen)
      (goto-char (point-min)))
    (with-current-buffer psy-buf
      (widen)
      (goto-char (point-min))
      (re-search-forward "^0001")
      (while (not (eobp))
        (let ((issn (buffer-substring-no-properties (point-at-bol) (+ 9 (point-at-bol))))
              (line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
              )
          (with-current-buffer jmd-buf
            (let ((here (point)))
              (unless (search-forward issn nil t)
                (goto-char here)
                (with-current-buffer out-buf
                  (insert line "\n"))))))
        (forward-line)))))

(defun pipm-pubmed-normalize (csv-file nrm-file)
  (let ((buf-csv (find-file-noselect csv-file))
        (buf-nrm (find-file-noselect nrm-file)))
    (with-current-buffer buf-nrm
      (unless (= 0 (buffer-size))
        (error "not empty")))
    (with-current-buffer buf-csv
      (widen)
      (goto-char (point-min))
      (while (re-search-forward "^JournalTitle: \\(.*\\)" nil t)
        (let ((name (match-string-no-properties 1))
              issn)
          (when (and name (re-search-forward "^ISSN: \\(.*\\)"))
            (setq issn (match-string-no-properties 1)))
          (when (and issn name)
            (with-current-buffer buf-nrm
              (insert issn "," name "\n")))
          )))))

(defun pipm-psychinfo-normalize (csv-file nrm-file)
  (let ((buf-csv (find-file-noselect csv-file))
        (buf-nrm (find-file-noselect nrm-file)))
    (with-current-buffer buf-nrm
      (unless (= 0 (buffer-size))
        (error "not empty")))
    (with-current-buffer buf-csv
      (widen)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((cols (split-string
                      (buffer-substring-no-properties (point-at-bol)
                                                      (point-at-eol))
                      ","))
               (name (nth 0 cols))
               (issn (nth 1 cols))
               )
          (when (and issn (< 0 (length issn)))
            (with-current-buffer buf-nrm
              (insert issn "," name "\n")))
          (forward-line)
          )))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pipm.el ends here
