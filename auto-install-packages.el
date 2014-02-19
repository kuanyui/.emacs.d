(require 'cl)
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

(setq dotfiles-packages-list
  '(ac-slime
    ace-jump-mode
    auto-complete
    auto-complete-nxml
    auto-indent-mode
    cl-lib
    column-enforce-mode
    company
    concurrent
    ctable
    deferred
    dired+
    dired-details
    dired-single
    discover
    enh-ruby-mode
    epc
    flymake-css
    flymake-easy
    flymake-haml
    flymake-python-pyflakes
    flymake-shell
    fringe-helper
    gh
    git-commit-mode
    git-gutter
    git-gutter-fringe
    git-rebase-mode
    god-mode
    goto-chg
    haml-mode
    helm
    highlight-defined
    highlight-indentation
    highlight-symbol
    hlinum
    ibuffer-vc
    jedi
    js-comint
    js2-mode
    json-mode
    linum-relative
    logito
    magit
    magit-commit-training-wheels
    magit-gh-pulls
    magit-push-remote
    makey
    markdown-mode
    minimap
    mmm-mode
    multiple-cursors
    pangu-spacing
    pcache
    popup
    powerline
    py-import-check
    python-info
    rainbow-delimiters
    rainbow-mode
    shell-command
    simple-httpd
    skewer-mode
    slime
    stylus-mode
    switch-window
    sws-mode
    twittering-mode
    undo-tree
    vlf
    web-beautify
    web-mode
    yagist
    yaml-mode
    yasnippet
    zlc
    ))

(defun dotfiles-auto-install-packages ()
  (interactive)
  (package-refresh-contents)
  (mapc #'(lambda (package)
            (unless (package-installed-p package)
              (package-install package)))
        dotfiles-packages-list))

(dotfiles-auto-install-packages)
(save-buffers-kill-emacs)
