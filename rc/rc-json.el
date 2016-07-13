;;; rc-json.el ---                                   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  kuanyui

;; Author: kuanyui <azazabc123@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defun comic-search-google ()
  (interactive)
  (let ((url (url-encode-url
              (concat "https://www.google.co.jp/search?q="
                      (buffer-substring-no-properties
                       (progn (right-char 1)
                              (re-search-backward "\"" nil t)
                              (right-char 1)
                              (point))
                       (progn (re-search-forward "\"" nil t)
                              (point))
                       )))))
    (eww url)))

(defun comic-insert-new-row ()
  (interactive)
  (save-excursion
    (insert "    {\"title\": \"\",
     \"jptitle\": \"\",
     \"author\": \"\",
     \"type\": 2,
     \"rating\": -1,
     \"progress\": \"已讀完\",
     \"status\": \"已完結/連載中\",
     \"comment\": \"\" },")))

(defun my-json-setup ()
  (rainbow-delimiters-mode-enable)
  (flycheck-mode 1)
  (define-key json-mode-map (kbd "<f3>") 'comic-search-google)
  (define-key json-mode-map (kbd "<f4>") 'comic-insert-new-row)
  )

(add-hook 'json-mode-hook 'my-json-setup)

(provide 'rc-json)
;;; rc-json.el ends here
