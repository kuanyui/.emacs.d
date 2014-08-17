(require 'cl)
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

(setq dotfiles-packages-list
      '(
	ac-js2
	ac-slime
	ace-jump-mode
	ack-and-a-half
	async
	auctex
	auto-complete
	calfw
	company
	concurrent
	ctable
	deferred
	dired+
	direx
	discover
	enh-ruby-mode
	epc
	flymake-css
	flymake-easy
	flymake-haml
	flymake-python-pyflakes
	flymake-shell
	gh
	git-commit-mode
	git-rebase-mode
	google-translate
	goto-chg
	haml-mode
	helm
	helm-ack
	helm-swoop
	highlight-symbol
	hlinum
	ht
	htmlize
	hungry-delete
	jedi
	jedi-direx
	js-comint
	js2-mode
	json-mode
	json-reformat
	json-snatcher
	log4e
	logito
	magit
	makey
	markdown-mode
	mediawiki
	mmm-mode
	multiple-cursors
	nodejs-repl
	org
	ox-html5slide
	pangu-spacing
	pcache
	pcre2el
	popup
	python-environment
	python-info
	qml-mode
	rainbow-delimiters
	rainbow-mode
	simple-httpd
	skewer-mode
	slime
	slime-company
	smart-operator
	stylus-mode
	swoop
	sws-mode
	twittering-mode
	undo-tree
	visual-regexp
	visual-regexp-steroids
	web-beautify
	web-mode
	yaml-mode
	yasnippet
	yaxception
        ))

(defun dotfiles-auto-install-packages ()
  (package-refresh-contents)
  (mapc #'(lambda (package)
            (unless (package-installed-p package)
              (package-install package)))
        dotfiles-packages-list)
  (save-buffers-kill-emacs))

(dotfiles-auto-install-packages)
