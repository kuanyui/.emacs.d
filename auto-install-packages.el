(require 'cl)
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

(setq dotfiles-packages-list
      '(ac-js2
        jedi
        smart-tabs-mode
        smart-operator
        ac-slime
        ace-jump-mode
        ack-and-a-half
        async
        auctex
        auto-complete
        auto-complete-auctex
        auto-complete-nxml
        auto-complete-pcmp
        auto-indent-mode
        column-enforce-mode
        company
        concurrent
        ctable
        deferred
        dired+
        dired-details
        dired-single
        discover
        eimp
        enh-ruby-mode
        epc
        expand-region
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
        helm-ack
        helm-swoop
        highlight-defined
        highlight-indentation
        highlight-symbol
        hlinum
        ht
        htmlize
        hungry-delete
        ibuffer-vc
        jedi
        js-comint
        js2-mode
        json-mode
        linum-relative
        log4e
        logito
        look-mode
        lusty-explorer
        mag-menu
        magit
        magit-commit-training-wheels
        magit-gh-pulls
        magit-push-remote
        makey
        markdown-mode
        minimap
        mmm-mode
        multiple-cursors
        org
        org-ac
        ox-html5slide
        pangu-spacing
        pcache
        pcre2el
        pcsv
        popup
        py-import-check
        python-info
        rainbow-delimiters
        rainbow-mode
        shell-command
        simple-httpd
        skewer-mode
        slime
        splitter
        stylus-mode
        switch-window
        swoop
        sws-mode
        twittering-mode
        undo-tree
        visual-regexp
        visual-regexp-steroids
        vlf
        web-beautify
        web-mode
        xclip
        yagist
        yaml-mode
        yasnippet
        yaxception
        zlc
        ))

(defun dotfiles-auto-install-packages ()
  (package-refresh-contents)
  (mapc #'(lambda (package)
            (unless (package-installed-p package)
              (package-install package)))
        dotfiles-packages-list)
  (save-buffers-kill-emacs))

(dotfiles-auto-install-packages)
