;;; rc-site-lisp.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2014  kuanyui

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

(add-to-list 'load-path "/usr/share/emacs/site-lisp")

;; lilypond
(require 'lilypond-mode)
(add-to-list 'auto-mode-alist '("\\.ly$" . lilypond-mode))
(require 'ob-lilypond)			;org-label for Lilypond

(provide 'rc-site-lisp)
;;; rc-site-lisp.el ends here
