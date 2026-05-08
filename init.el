;;kuanyui's ~/.emacs

;; (setq warning-minimum-level :emergency)

;;掃描~/.emacs.d目錄
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(package-initialize)
(require 'benchmark-init)
(add-hook 'after-init-hook 'benchmark-init/deactivate)

(add-to-list 'load-path "~/.emacs.d/rc")
;; (profiler-start 'cpu)
;; (profiler-stop)
;; (profiler-report)
(require 'rc-basic)
(require 'rc-programming)
(require 'rc-web-development)
(require 'rc-templates)
(require 'rc-edit)
(require 'rc-hloccur)
(require 'rc-dired)
(require 'rc-ibuffer)
(require 'rc-eshell)
(require 'rc-calendar)
(require 'rc-term)

;; (require 'rc-gnus)
;; (require 'rc-twittering)

(require 'rc-magit)
;; (if (member system-type '(darwin gnu/linux))
;; (require 'rc-emms)
;; )
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
(require 'rc-makefile)
(require 'rc-polymode)

(require 'rc-my)

(require 'rc-junk)

(require 'rc-site-lisp)

(message (emacs-init-time))
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
   '("8aed2e3a45ddf6e52ca107cea321f48d3f70713d2e7e5053f9d07502c9f4a23c"
     "2d507213fd8b11e8d3db74eab19d3836dd9efc4c9c64e808ba521aea408dd031"
     "9820d60186991c88d161653cd9b300697091ff8a7de90f0fcd678ff7ed1a0af0"
     "c92baa556823c45f5320ed087e0db9093d785b9cd8bd5d63e61661632520e5d6"
     "ea25088829a4b234eaff8912f226df503ab66db86cdc0c89a440848cbd448707"
     "07373425a2edce47f60ec0ed07c6ac44c1802c67991064604a4aa0428194b0d6"
     "9c5e3dce6768038ba77c9529526f230fa001174abaac1f7e85a50f7af5be5bb3"
     "03dfd9158d921f45c71b00863434b025dd6cb884ce7777d554f4fe3cb5da8e68"
     "c6cc8a612a77c9fb96f9b5c69009e9e2bbaf6be66887a689917c88a68c11cd28"
     default))
 '(delete-selection-mode nil)
 '(diff-hl-margin-mode t)
 '(flycheck-javascript-flow-args nil)
 '(global-diff-hl-mode t)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(guide-key-mode nil)
 '(helm-mode nil)
 '(large-file-warning-threshold nil)
 '(line-number-mode nil)
 '(mark-even-if-inactive t)
 '(org-agenda-files '("~/org/agenda/Event.org" "~/org/agenda/Todo.org"))
 '(org-modules
   '(org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc
	      org-mhe org-rmail org-w3m))
 '(package-selected-packages
   '(ac-haskell-process
     ac-inf-ruby ac-js2 ac-slime ace-jump-mode ack ag
     aggressive-indent anaconda-mode bbdb-
     bbdb-csv-import benchmark-init calfw cmake-ide
     cmake-mode col-highlight column-marker company
     company-anaconda company-c-headers
     company-flow company-jedi diff-hl dired+ direx
     discover duplicate-thing editorconfig elpy
     emmet-mode emms-player-mpv emms-state emr
     enh-ruby-mode eslint-fix esqlite esup
     expand-region f fiplr flow-minor-mode flx-ido
     flycheck flycheck-flow flymake-css
     flymake-haml flymake-json
     flymake-python-pyflakes flymake-shell forge
     geiser ggtags gh git-modes git-timemachine
     go-mode golint google-translate goto-chg
     haml-mode helm helm-core helm-projectile
     highlight-symbol hl-todo htmlize hungry-delete
     ibuffer-projectile indent-guide jade-mode
     jedi-core js-comint js2-mode js2-refactor
     json-mode less-css-mode lua-mode magit
     manage-minor-mode marginalia markdown-mode
     mediawiki mmm-mode move-text multiple-cursors
     neotree nodejs-repl oauth orderless org-roam
     ox-html5slide pangu-spacing paradox php-mode
     powerline projectile pug-mode
     py-smart-operator pylint python-info pyvenv
     rainbow-delimiters rainbow-identifiers
     rainbow-mode request rich-minority scss-mode
     sicp slime-company smart-operator smart-tab
     smartparens smooth-scrolling sql-indent
     sqlup-mode sr-speedbar stylus-mode
     sudden-death swoop toc-org treemacs tuareg
     twittering-mode undo-tree vertico
     virtualenvwrapper visual-regexp
     visual-regexp-steroids vue-mode web-beautify
     web-mode wgrep-ack wgrep-ag xterm-color
     yaml-mode))
 '(paradox-github-token t)
 '(safe-local-variable-directories '("/etc/"))
 '(safe-local-variable-values
   '((css-indent-offset . 2) (js2-strict-missing-semi-warning)
     (eval progn
	   (when (fboundp 'aggressive-indent-mode)
	     (aggressive-indent-mode -1)))
     (auto-revert-mode . t) (eval progn (aggressive-indent-mode -1))
     (aggressive-indent-mode) (js2-indent-switch-body . t)
     (org-html-allow-name-attribute-in-anchors) (org-html-style . "")
     (org-html-toplevel-hlevel . 1) (org-html-with-latex)
     (org-html-use-infojs) (org-html-infojs-template . "")
     (org-html-mathjax-template . "") (org-html-postamble)
     (org-html-preamble) (org-html-head-include-scripts)
     (org-export-with-toc) (org-export-with-latex)
     (org-export-time-stamp-file) (org-export-with-section-numbers)
     (org-export-with-title) (org-export-with-email)
     (org-export-with-date) (org-export-with-creator)
     (org-export-with-author) (major-mode . org-mode)
     (major-mode . org) (org-html-link-org-files-as-html)
     (pangu-spacing-real-insert-separtor . t) (pangu-spacing-mode . t)))
 '(transient-mark-mode 1)
 '(warning-suppress-log-types '((editorconfig)))
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(xclip-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
