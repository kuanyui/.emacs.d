;;kuanyui's ~/.emacs

;;掃描~/.emacs.d目錄
(add-to-list 'load-path "~/.emacs.d/rc")
(require 'rc-basic)
(require 'rc-programming)
(require 'rc-templates)
(require 'rc-edit)
(require 'rc-dired)
(require 'rc-ibuffer)
(require 'rc-eshell)

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



;;======================================================
;; customize 以下為Emacs自動生成，不要動
;;======================================================
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("420efa03d9c9c519a4a7eca29918805387b8a7709e23b283f64a3163403e6b7d" "47160226d9b534d26067fcfb07f6bc1ea5922d9caafd29536c1a212e1cef571b" "7fde61efa16011b294db1448de9e0ae45d602ae949a640164bce6fece4420e90" "3f3410aaa7417bddb85bc29cadb34ccdcf579b2f4126d42d4bf07ef270d2fbba" "7ed6913f96c43796aa524e9ae506b0a3a50bfca061eed73b66766d14adfa86d1" "ea3f08f94c7732c63ffc601f5fcd2632d1b99f9195bf3b55f4710a37fd985d04" "dbfa6f95b6e56fb7b1592f610583e87ebb16d3e172416a107f0aceef1351aad0" "9ba004f6d3e497c9f38859ae263b0ddd3ec0ac620678bc291b4cb1a8bca61c14" "6aae982648e974445ec8d221cdbaaebd3ff96c3039685be9207ca8ac6fc4173f" default)))
 '(delete-selection-mode nil)
 '(guide-key-mode nil)
 '(helm-mode nil)
 '(mark-even-if-inactive t)
 '(org-agenda-files
   (quote
    ("~/org/agenda/Todo.org" "~/org/agenda/School.org" "~/org/agenda/Reading.org" "~/org/agenda/Project.org" "~/org/agenda/Learning.org" "~/org/agenda/Habit.org" "~/org/agenda/Event.org")))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(resize-frame nil)
 '(scroll-bar-mode (quote right))
 '(transient-mark-mode 1)
 '(xclip-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'dired-find-alternate-file 'disabled nil)
 
