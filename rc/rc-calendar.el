;;; rc-calendar.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2015  kuanyui

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
(add-to-list 'load-path "~/.emacs.d/git/taiwan-calendar/")
(require 'taiwan-calendar)
(setq mark-holidays-in-calendar t)
(setq taiwan-calendar-important-holidays taiwan-calendar-taiwan-holidays)
(setq calendar-holidays taiwan-calendar-important-holidays)


(provide 'rc-calendar)
;;; rc-calendar.el ends here
