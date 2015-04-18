;;kuanyui's ~/.emacs

;;掃描~/.emacs.d目錄
(add-to-list 'load-path "~/.emacs.d/rc")
(require 'rc-basic)
(require 'rc-programming)
(require 'rc-web-development)
(require 'rc-templates)
(require 'rc-edit)
(require 'rc-dired)
(require 'rc-ibuffer)
(require 'rc-eshell)
(require 'rc-calendar)

(require 'rc-gnus)
(require 'rc-twittering)
(require 'rc-magit)

(require 'rc-org)
(require 'rc-markdown)

(require 'rc-misc)
(require 'rc-private)

(require 'rc-python)
(require 'rc-haskell)
(require 'rc-ruby)
(require 'rc-perl)
(require 'rc-lisp)
(require 'rc-css)
(require 'rc-javascript)
(require 'rc-shell)
(require 'rc-qml)
(require 'rc-c)

(require 'rc-junk)

(require 'rc-site-lisp)

;;======================================================
;; customize 以下為Emacs自動生成，不要動
;;======================================================
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(guide-key-mode nil)
 '(helm-mode nil)
 '(mark-even-if-inactive t)
 '(org-agenda-files (quote ("~/org/agenda/Event.org" "~/org/agenda/Todo.org")))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(paradox-github-token t)
 '(safe-local-variable-values
   (quote
    ((major-mode . org-mode)
     (major-mode . org)
     (org-html-link-org-files-as-html)
     (pangu-spacing-real-insert-separtor . t)
     (pangu-spacing-mode . t))))
 '(scroll-bar-mode (quote right))
 '(transient-mark-mode 1)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(xclip-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'dired-find-alternate-file 'disabled nil)
