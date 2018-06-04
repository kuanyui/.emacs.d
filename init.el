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
(require 'rc-term)

(require 'rc-gnus)
(require 'rc-twittering)
(require 'rc-magit)
(if (member system-type '(darwin gnu/linux))
    (require 'rc-emms)
  )
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
(require 'rc-kolor)
(require 'rc-js)
(require 'rc-json)
(require 'rc-shell)
(require 'rc-qml)
(require 'rc-c)
(require 'rc-go)
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
    ("ea25088829a4b234eaff8912f226df503ab66db86cdc0c89a440848cbd448707" "07373425a2edce47f60ec0ed07c6ac44c1802c67991064604a4aa0428194b0d6" "9c5e3dce6768038ba77c9529526f230fa001174abaac1f7e85a50f7af5be5bb3" "03dfd9158d921f45c71b00863434b025dd6cb884ce7777d554f4fe3cb5da8e68" "c6cc8a612a77c9fb96f9b5c69009e9e2bbaf6be66887a689917c88a68c11cd28" default)))
 '(delete-selection-mode nil)
 '(flycheck-javascript-flow-args nil)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(guide-key-mode nil)
 '(helm-mode nil)
 '(large-file-warning-threshold nil)
 '(mark-even-if-inactive t)
 '(org-agenda-files (quote ("~/org/agenda/Event.org" "~/org/agenda/Todo.org")))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(package-selected-packages
   (quote
    (js2-mode company-flow flycheck-flow manage-minor-mode flow-minor-mode golint column-marker col-highlight pug-mode anaconda-mode company-anaconda jedi-core firefox-controller flymake-json editorconfig vue-mode multiple-cursors helm-dash helm-ag helm-projectile projectile yaml-mode xterm-color wgrep-ag wgrep-ack web-mode web-beautify visual-regexp-steroids virtualenvwrapper undo-tree twittering-mode tuareg swoop sudden-death stylus-mode sr-speedbar sqlup-mode sql-indent smooth-scrolling smartparens smart-tab smart-operator slime-company sicp scss-mode rich-minority rainbow-mode rainbow-identifiers rainbow-delimiters python-info pylint py-smart-operator powerline php-mode paradox pangu-spacing ox-html5slide oauth nodejs-repl neotree mmm-mode mediawiki markdown-mode magit less-css-mode json-mode js2-refactor js-comint jade-mode indent-guide ibuffer-projectile hungry-delete htmlize hlinum highlight-symbol helm-gtags haml-mode goto-chg google-translate go-mode git-gutter-fringe gh ggtags geiser flymake-shell flymake-python-pyflakes flymake-haml flymake-css flycheck flx-ido fiplr f esup esqlite enh-ruby-mode emr emms-state emms-player-mpv emmet-mode elpy discover direx dired+ company-jedi company-c-headers cmake-mode cmake-ide calfw bbdb-csv-import bbdb- aggressive-indent ag ack ace-jump-mode ac-slime ac-js2 ac-inf-ruby ac-haskell-process)))
 '(paradox-github-token t)
 '(safe-local-variable-values
   (quote
    ((auto-revert-mode . t)
     (eval progn
           (aggressive-indent-mode -1))
     (aggressive-indent-mode)
     (js2-indent-switch-body . t)
     (org-html-allow-name-attribute-in-anchors)
     (org-html-style . "")
     (org-html-toplevel-hlevel . 1)
     (org-html-with-latex)
     (org-html-use-infojs)
     (org-html-infojs-template . "")
     (org-html-mathjax-template . "")
     (org-html-postamble)
     (org-html-preamble)
     (org-html-head-include-scripts)
     (org-export-with-toc)
     (org-export-with-latex)
     (org-export-time-stamp-file)
     (org-export-with-section-numbers)
     (org-export-with-title)
     (org-export-with-email)
     (org-export-with-date)
     (org-export-with-creator)
     (org-export-with-author)
     (eval progn
           (require
            (quote projectile))
           (setq-local flycheck-pylintrc
                       (concat
                        (projectile-project-root)
                        ".pylintrc")))
     (eval progn
           (setq-local python-environment-directory
                       (concat default-directory "venv/")))
     (jedi:environment-root . "venv/")
     (jedi:environment-root . "./venv/")
     (js2-strict-missing-semi-warning)
     (eval when
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
