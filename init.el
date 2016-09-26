;;kuanyui's ~/.emacs

;;掃描~/.emacs.d目錄

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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
(require 'rc-emms)
(require 'rc-eww)

(require 'rc-org)
(require 'rc-markdown)

(require 'rc-misc)
(require 'rc-private)

(require 'rc-python)
(require 'rc-haskell)
(require 'rc-ruby)
(require 'rc-perl)
(require 'rc-lisp)
(require 'rc-scheme)
(require 'rc-css)
(require 'rc-js)
(require 'rc-json)
(require 'rc-shell)
(require 'rc-qml)
(require 'rc-c)
(require 'rc-sql)
(require 'rc-qt)

(require 'rc-my)

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
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("03dfd9158d921f45c71b00863434b025dd6cb884ce7777d554f4fe3cb5da8e68" "c6cc8a612a77c9fb96f9b5c69009e9e2bbaf6be66887a689917c88a68c11cd28" default)))
 '(delete-selection-mode nil)
 '(guide-key-mode nil)
 '(helm-mode nil)
 '(mark-even-if-inactive t)
 '(org-agenda-files (quote ("~/org/agenda/Event.org" "~/org/agenda/Todo.org")))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(package-selected-packages
   (quote
    (helm-dash helm-ag helm-projectile projectile yaml-mode xterm-color wgrep-ag wgrep-ack web-mode web-beautify visual-regexp-steroids virtualenvwrapper undo-tree twittering-mode tuareg swoop sudden-death stylus-mode sr-speedbar sqlup-mode sql-indent smooth-scrolling smartparens smart-tab smart-operator slime-company sicp scss-mode rich-minority rainbow-mode rainbow-identifiers rainbow-delimiters qml-mode python-info pylint py-smart-operator powerline php-mode paradox pangu-spacing ox-html5slide oauth nodejs-repl neotree mmm-mode mediawiki markdown-mode magit less-css-mode json-mode js2-refactor js-comint jade-mode indent-guide ibuffer-projectile hungry-delete htmlize hlinum highlight-symbol helm-gtags haml-mode goto-chg google-translate go-mode git-gutter-fringe gh ggtags geiser flymake-shell flymake-python-pyflakes flymake-haml flymake-css flycheck flx-ido fiplr f esup esqlite enh-ruby-mode emr emms-state emms-player-mpv emmet-mode elpy discover direx dired+ company-jedi company-c-headers coffee-mode cmake-mode cmake-ide calfw bbdb-csv-import bbdb- aggressive-indent ag ack ace-jump-mode ac-slime ac-js2 ac-inf-ruby ac-haskell-process)))
 '(paradox-github-token t)
 '(safe-local-variable-values
   (quote
    ((eval when
           (and
            (buffer-file-name)
            (file-regular-p
             (buffer-file-name))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (emacs-lisp-mode)
           (when
               (fboundp
                (quote flycheck-mode))
             (flycheck-mode -1))
           (unless
               (featurep
                (quote package-build))
             (let
                 ((load-path
                   (cons ".." load-path)))
               (require
                (quote package-build))))
           (package-build-minor-mode)
           (set
            (make-local-variable
             (quote package-build-working-dir))
            (expand-file-name "../working/"))
           (set
            (make-local-variable
             (quote package-build-archive-dir))
            (expand-file-name "../packages/"))
           (set
            (make-local-variable
             (quote package-build-recipes-dir))
            default-directory))
     (major-mode . org-mode)
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
