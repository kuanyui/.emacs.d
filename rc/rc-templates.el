;;; rc-templates.el ---                              -*- lexical-binding: t; -*-

;;; Code:

;;======================================================
;; Yasnippet
;;======================================================

;;自分用のスニペットフォルダと，拾ってきたスニペットフォルダの2つを作っておきます．
;;(一つにまとめてもいいけど)
(require 'yasnippet)
;; (yas-global-mode 1)
(global-set-key (kbd "C-c s n") 'yas-new-snippet)
(global-set-key (kbd "C-c s i") 'yas-insert-snippet)
(global-set-key (kbd "C-c s v") 'yas-visit-snippet-file)

(setq auto-revert-interval 1)


(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
	 (1+ (current-column))))))


(define-key prog-mode-map (kbd "C-c f") 'toggle-selective-display)

(add-hook 'markdown-mode-hook
	  (lambda ()
	    (yas-minor-mode -1)		;Disable Yasnippet in markdown-mode
	    (define-key markdown-mode-map (kbd "TAB") 'markdown-cycle) ;Tab to fold title
	    (markdown-cycle t) ;Auto fold all title after startuping markdown-mode.
	    ))

;;======================================================
;; `auto-insert' Templates
;;======================================================
(require 'autoinsert)
(setq auto-insert-query nil)
(setq auto-insert-directory "~/.emacs.d/templates/")
(auto-insert-mode t)

;; Python
(define-auto-insert
  '("\\.py\\'" . "Python")
  '(nil
    "# -*- coding: utf-8 -*-\n"
    ))

;; Org
(define-auto-insert
  '("\\.org\\'" . "Org")
  '(nil
    "#+TITLE: " (read-from-minibuffer "Title: " (replace-regexp-in-string "\\(^.+\\)\.org$" "\\1" (buffer-real-name))) "\n"
    "#+DATE: " (format-time-string "%Y/%m/%d（%a）%H:%M") "\n"
    "#+AUTHOR: " user-full-name "\n"
    "#+EMAIL: " user-mail-address "\n"
    "#+OPTIONS: ':nil *:t -:t ::t <:t H:3 \\n:nil ^:t arch:headline\n"
    "#+OPTIONS: author:t c:nil creator:comment d:(not \"LOGBOOK\") date:t\n"
    "#+OPTIONS: e:t email:nil f:t inline:t num:t p:nil pri:nil stat:t\n"
    "#+OPTIONS: tags:t tasks:t tex:t timestamp:t toc:nil todo:t |:t\n"
    "#+CREATOR: " (format "Emacs %s (Org mode %s)"
                          emacs-version (org-version nil nil)) "\n"
    "#+DESCRIPTION:\n"
    "#+EXCLUDE_TAGS: noexport\n"
    "#+KEYWORDS:\n"
    "#+LANGUAGE: en\n"
    "#+SELECT_TAGS: export\n"
    ))

;; gitignore
(defun touch-gitignore ()
  (interactive)
  (let* ((file (concat
                (read-directory-name "Place ignore file to: " nil nil) ".gitignore")))
    (if (file-exists-p file)
        (message ".gitignore file has been exist, abort.")
      (progn
        (copy-file (concat auto-insert-directory "template.gitignore") file)
        (message "Done.")))))




(provide 'rc-templates)
;;; rc-templates.el ends here
